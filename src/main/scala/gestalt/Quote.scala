package scala.gestalt

import scala.collection.immutable.Seq
import scala.{meta => m}
import scala.compat.Platform.EOL

object Flatten {
  def unapply[T](x: Option[Seq[T]]): Option[Seq[T]] = x match {
    case Some(xs) => Some(xs)
    case None => Some(Nil)
  }
}

/** Lift scala.meta trees as t.trees */
abstract class Quote(val t: Toolbox, val toolboxName: String) {
  import Quasiquote.Hole

  type Quasi = m.internal.ast.Quasi

  // fields
  def args: List[t.Tree]
  def isTerm: Boolean

  // helpers
  private implicit class TreeOps(val tree: t.Tree) {
    def select(name: String): t.Tree = t.Select(tree, name)

    def appliedTo(args: t.Tree*): t.Tree = t.Apply(tree, args.toList)

    def appliedToType(args: t.TypeTree*): t.Tree = t.ApplyType(tree, args.toList)
  }

  // lifted trees
  lazy val scalaNil  = select("scala.Nil")
  lazy val scalaList = select("scala.List")
  lazy val scalaSome = select("scala.Some")
  lazy val scalaNone = select("scala.None")
  lazy val toolbox   = t.Ident(toolboxName)
  lazy val root      = t.Ident("_root_")

  private def select(path: String, isTerm: Boolean = true): t.Tree = {
    val parts = path.split('.')

    val qual = parts.init.foldLeft[t.Tree](root) { (prefix, name) =>
      prefix.select(name)
    }

    if (isTerm) t.Select(qual, parts.last) else t.TypeSelect(qual, parts.last)
  }

  private def selectToolbox(path: String): t.Tree = {
    val parts = path.split('.')

    val qual = parts.init.foldLeft[t.Tree](toolbox) { (prefix, name) =>
      prefix.select(name)
    }

    if (isTerm) t.Select(qual, parts.last) else t.TypeSelect(qual, parts.last)
  }

  // lifts: m.Tree => t.Tree[t.Tree]

  def liftSeq(trees: Seq[m.Tree]): t.Tree =  {
    def loop(trees: List[m.Tree], acc: Option[t.Tree], prefix: List[m.Tree]): t.Tree = trees match {
      case (quasi: Quasi) +: rest if quasi.rank == 1 =>
        if (acc.isEmpty) {
          if (prefix.isEmpty) loop(rest, Some(liftQuasi(quasi)), Nil)
          else loop(rest, Some(prefix.foldRight(liftQuasi(quasi))((curr, acc) => {
            val currElement = lift(curr)
            t.Infix(currElement, "+:", acc)
          })), Nil)
        } else {
          require(prefix.isEmpty)
          if (isTerm) loop(rest, Some(t.Infix(acc.get, "++", liftQuasi(quasi))), Nil)
          else {
            t.error(m.internal.parsers.Messages.QuasiquoteAdjacentEllipsesInPattern(quasi.rank))
          }
        }
      case other +: rest =>
        if (acc.isEmpty) loop(rest, acc, prefix :+ other)
        else {
          require(prefix.isEmpty)
          loop(rest, Some(t.Infix(acc.get, ":+", lift(other))), Nil)
        }
      case Nil =>
        if (acc.isEmpty)
          scalaList.appliedTo(prefix.map(lift): _*)
        else acc.get
    }

    loop (trees.toList, None, Nil)
  }

  def liftSeqSeq(treess: Seq[Seq[m.Tree]]): t.Tree = {
    val tripleDotQuasis = treess.flatten.collect{ case quasi: Quasi if quasi.rank == 2 => quasi }
    if (tripleDotQuasis.length == 0) {
      val args = treess.map(liftSeq)
      scalaList.appliedTo(args: _*)
    } else if (tripleDotQuasis.length == 1) {
      if (treess.flatten.length == 1) liftQuasi(tripleDotQuasis(0))
      else {
        t.error("implementation restriction: can't mix ...$ with anything else in parameter lists." +
          EOL + "See https://github.com/scalameta/scalameta/issues/406 for details.")
      }
    } else {
      t.error(m.internal.parsers.Messages.QuasiquoteAdjacentEllipsesInPattern(2))
    }
  }

  def liftOpt(treeOpt: Option[m.Tree]): t.Tree = treeOpt match {
    case Some(quasi: Quasi) =>
      liftQuasi(quasi, optional = true)
    case Some(tree) =>
      scalaSome.appliedTo(lift(tree))
    case None =>
      scalaNone
  }

  def liftOptSeq(treesOpt: Option[Seq[m.Tree]]): t.Tree = treesOpt match {
    case Some(Seq(quasi: Quasi)) if quasi.rank > 0 && !isTerm =>
      select("scala.gestalt.Flatten").appliedTo(liftQuasi(quasi))
    case Some(trees) =>
      scalaSome.appliedTo(liftSeq(trees))
    case None =>
      scalaNone
  }

  def liftQuasi(quasi: Quasi, expectedRank: Int = 0, optional: Boolean = false): t.Tree = {
    if (quasi.rank > 0) return liftQuasi(quasi.tree.asInstanceOf[Quasi], quasi.rank, optional)

    def arg(i: Int) =
      if (!isTerm && optional) scalaSome.appliedTo(args(i))
      else args(i)

    quasi.tree match {
      case m.Term.Name(Hole(i)) => arg(i)
      case m.Type.Name(Hole(i)) => arg(i)
    }
  }

  /** Lift initcall : qual.T[A, B](x, y)(z) */
  def liftInitCall(tree: m.Tree): t.Tree = {
    def extractFun(tree: m.Tree): (t.Tree, t.Tree, t.Tree) = tree match {
      case m.Type.Apply(m.Ctor.Ref.Select(qual, m.Ctor.Ref.Name(name)), targs) =>
        (scalaSome.appliedTo(lift(qual)), t.Lit(name), liftSeq(targs))
      case m.Type.Apply(m.Ctor.Ref.Name(name), targs) =>
        (scalaNone, t.Lit(name), liftSeq(targs))
      case m.Ctor.Ref.Name(name) =>
        (scalaNone, t.Lit(name), liftSeq(Nil))
    }

    def initCall(ctor: m.Tree, argss: Seq[Seq[m.Tree]]): t.Tree = {
      val (qualOpt, name, targs) = extractFun(ctor)
      selectToolbox("InitCall").appliedTo(qualOpt, name, targs, liftSeqSeq(argss))
    }

    tree match {
      case m.internal.ast.Helpers.TermApply(ctor, argss) =>
        initCall(ctor, argss)
      case _ =>
        initCall(tree, Nil)
    }
  }

  def liftSeqTrees(trees: Seq[t.Tree]): t.Tree = trees match {
    case head :: rest => t.Infix(head, "::", liftSeqTrees(rest))
    case _ => scalaNil
  }

  def liftValDef(mods: t.Tree, pats: Seq[m.Pat], tpe: t.Tree, rhs: t.Tree): t.Tree = {
    if (pats.size == 1) {
      val left = pats(0) match {
        case quasi: Quasi =>
          liftQuasi(quasi)
        case m.Term.Name(name) =>
          t.Lit(name)
        case pat =>
          lift(pat)
      }

      selectToolbox("ValDef").appliedTo(mods, left, tpe, scalaNone)
    }
    else
      selectToolbox("ValDef").appliedTo(mods, liftSeq(pats), tpe, rhs)
  }

  /** Lift self annotation in class definition */
  def liftSelf(tree: m.Tree): t.Tree = tree match {
    case m.Term.Param(_, m.Name.Anonymous(), _, _) =>
      scalaNone
    case m.Term.Param(_, m.Term.Name(name), tpOpt, _) =>
      scalaSome appliedTo selectToolbox("Self").appliedTo(t.Lit(name), liftOpt(tpOpt))
  }

  // lift name to either Lit or Quasi
  def liftName(name: m.Name): t.Tree = name match {
    case quasi: Quasi => liftQuasi(quasi)
    case _ => t.Lit(name.value)
  }

  def lift(tree: m.Tree): t.Tree = tree match {
    case quasi: Quasi  =>
      liftQuasi(quasi)

    case m.Lit(value) => t.Lit(value)

    // case m.Name.Anonymous() =>
    // case m.Name.Indeterminate(name) =>

    case m.Term.This(qual)  =>
      val name = qual match {
        case m.Name.Anonymous() => ""
        case m.Name.Indeterminate(name) => name
      }

      selectToolbox("This").appliedTo(t.Lit(name))
    case m.Term.Super(thisp, superp) =>
      val qual = thisp match {
        case m.Name.Anonymous() => ""
        case m.Name.Indeterminate(name) => name
      }

      val mix = superp match {
        case m.Name.Anonymous() => ""
        case m.Name.Indeterminate(name) => name
      }
      selectToolbox("Super").appliedTo(t.Lit(qual), t.Lit(mix))
    case m.Term.Name(name) =>
      selectToolbox("Ident").appliedTo(t.Lit(name))
    case m.Term.Select(qual, m.Term.Name(name)) =>
      selectToolbox("Select").appliedTo(lift(qual), t.Lit(name))
    case m.Term.Interpolate(m.Term.Name(tag), parts, args) =>
      selectToolbox("Interpolate").appliedTo(t.Lit(tag), liftSeq(parts), liftSeq(args))
    // case m.Term.Xml(parts, args) =>
    case m.Term.Apply(fun, args) =>
      // magic happens here with ...$args
      args match {
        case Seq(quasi: Quasi) if quasi.rank == 2 =>
          selectToolbox("ApplySeq").appliedTo(lift(fun), liftQuasi(quasi))
        case _ =>
          selectToolbox("Apply").appliedTo(lift(fun), liftSeq(args))
      }
    case m.Term.ApplyInfix(lhs, m.Term.Name(name), targs, args) =>
      require(targs.length == 0) // no targs for now
      if (args.size == 1)
        selectToolbox("Infix").appliedTo(lift(lhs), t.Lit(name), lift(args(0)))
      else
        selectToolbox("Infix").appliedTo(lift(lhs), t.Lit(name), selectToolbox("Tuple").appliedTo(liftSeq(args)))
    case m.Term.ApplyType(fun, targs) =>
      selectToolbox("ApplyType").appliedTo(lift(fun), liftSeq(targs))
    case m.Term.ApplyUnary(m.Term.Name(op), arg) =>
      selectToolbox("Prefix").appliedTo(t.Lit(op), lift(arg))
    case m.Term.Assign(lhs, rhs) =>
      selectToolbox("Assign").appliedTo(lift(lhs), lift(rhs))
    case m.Term.Update(fun, argss, rhs) =>
      require(argss.size > 0)
      val left = selectToolbox("ApplySeq").appliedTo(lift(fun), liftSeqSeq(argss))
      selectToolbox("Assign").appliedTo(left, lift(rhs))
    case m.Term.Return(expr) =>
      selectToolbox("Return").appliedTo(lift(expr))
    case m.Term.Throw(expr) =>
      selectToolbox("Throw").appliedTo(lift(expr))
    case m.Term.Ascribe(expr, tpe) =>
      selectToolbox("Ascribe").appliedTo(lift(expr), lift(tpe))
    case m.Term.Annotate(expr, annots) =>
      selectToolbox("Annotated").appliedTo(lift(expr), liftSeq(annots))
    case m.Term.Tuple(args) =>
      selectToolbox("Tuple").appliedTo(liftSeq(args))
    case m.Term.Block(stats) =>
      selectToolbox("Block").appliedTo(liftSeq(stats))
    case m.Term.If(cond, thenp, elsep) =>
      selectToolbox("If").appliedTo(lift(cond), lift(thenp), lift(elsep))
    case m.Term.Match(expr, cases) =>
      selectToolbox("Match").appliedTo(lift(expr), liftSeq(cases))
    case m.Term.TryWithCases(expr, catchp, finallyp) =>
      selectToolbox("Try").appliedTo(lift(expr), liftSeq(catchp), liftOpt(finallyp))
    case m.Term.TryWithTerm(expr, catchp, finallyp) =>
      selectToolbox("Try").appliedTo(lift(expr), lift(catchp), liftOpt(finallyp))
    case m.Term.Function(params, body) =>
      selectToolbox("Function").appliedTo(liftSeq(params), lift(body))
    case m.Term.PartialFunction(cases) =>
      selectToolbox("PartialFunction").appliedTo(liftSeq(cases))
    case m.Term.While(expr, body) =>
      selectToolbox("While").appliedTo(lift(expr), lift(body))
    case m.Term.Do(body, expr) =>
      selectToolbox("DoWhile").appliedTo(lift(body), lift(expr))
    case m.Term.For(enums, body) =>
      selectToolbox("For").appliedTo(liftSeq(enums), lift(body))
    case m.Term.ForYield(enums, body) =>
      selectToolbox("For").appliedTo(liftSeq(enums), selectToolbox("Yield").appliedTo(lift(body)))
    case m.Term.New(m.Template(Nil, Seq(mctor), m.Term.Param(Nil, m.Name.Anonymous(), None, None), None)) =>
      selectToolbox("New").appliedTo(liftInitCall(mctor))
    case m.Term.New(m.Template(_, parents, self, stats)) =>
      val parentCalls = liftSeqTrees(parents.map(liftInitCall))
      val anonym = selectToolbox("AnonymClass").appliedTo(parentCalls, liftSelf(self), liftOptSeq(stats))
      selectToolbox("New").appliedTo(anonym)
    case m.Term.Placeholder() =>
      selectToolbox("Wildcard").appliedTo()
    case m.Term.Eta(expr) =>
      selectToolbox("Postfix").appliedTo(lift(expr), t.Lit("_"))
    case m.Term.Arg.Named(m.Term.Name(name), expr) =>
      selectToolbox("Named").appliedTo(t.Lit(name), lift(expr))
    case m.Term.Arg.Repeated(expr) =>
      selectToolbox("Repeated").appliedTo(lift(expr))
    case m.Term.Param(mods, m.Term.Name(name), tpe, default) =>
      selectToolbox("Param").appliedTo(liftSeq(mods), t.Lit(name), liftOpt(tpe), liftOpt(default))

    // types
    case m.Type.Name(name) =>
      selectToolbox("TypeIdent").appliedTo(t.Lit(name))
    case m.Type.Select(qual, m.Type.Name(name)) =>
      selectToolbox("TypeSelect").appliedTo(lift(qual), t.Lit(name))
    // case m.Type.Project(qual, name) =>
    case m.Type.Singleton(ref) =>
      selectToolbox("TypeSingleton").appliedTo(lift(ref))
    case m.Type.Apply(tpe, args) =>
      selectToolbox("TypeApply").appliedTo(lift(tpe), liftSeq(args))
    case m.Type.ApplyInfix(lhs, m.Type.Name(op), rhs) =>
      selectToolbox("ApplyInfix").appliedTo(lift(lhs), t.Lit(op), lift(rhs))
    case m.Type.Function(params, res) =>
      selectToolbox("Function").appliedTo(liftSeq(params), lift(res))
    case m.Type.Tuple(args) =>
      selectToolbox("TypeTuple").appliedTo(liftSeq(args))
    // case m.Type.With(lhs, rhs) =>
    case m.Type.And(lhs, rhs) =>
      selectToolbox("TypeAnd").appliedTo(lift(lhs), lift(rhs))
    case m.Type.Or(lhs, rhs) =>
      selectToolbox("TypeOr").appliedTo(lift(lhs), lift(rhs))
    case m.Type.Refine(tpe, stats) =>
      selectToolbox("TypeRefine").appliedTo(liftOpt(tpe), liftSeq(stats))
    // case m.Type.Existential(tpe, stats) =>
    case m.Type.Annotate(tpe, annots) =>
      selectToolbox("TypeAnnotated").appliedTo(lift(tpe), liftSeq(annots))
    case m.Type.Placeholder(bounds) =>
      selectToolbox("TypeWildcard").appliedTo(lift(bounds))
    case m.Type.Bounds(lo, hi) =>
      selectToolbox("TypeBounds").appliedTo(liftOpt(lo), liftOpt(hi))
    case m.Type.Arg.ByName(tpe) =>
      selectToolbox("TypeByName").appliedTo(lift(tpe))
    case m.Type.Arg.Repeated(tpe) =>
      selectToolbox("TypeRepeated").appliedTo(lift(tpe))
    case m.Type.Param(mods, name, tparams, tbounds, vbounds, cbounds) =>
      require(vbounds.size == 0)
      val nameStr = name match {
        case m.Name.Anonymous() => "_"
        case m.Type.Name(name)  => name
      }

      selectToolbox("TypeParam").appliedTo(
        liftSeq(mods), t.Lit(nameStr), liftSeq(tparams), lift(tbounds), scalaNil, liftSeq(cbounds)
      )

    // patterns
    case m.Pat.Var.Term(m.Term.Name(name)) =>
      selectToolbox("Ident").appliedTo(t.Lit(name))
    case m.Pat.Var.Type(m.Type.Name(name)) =>
      selectToolbox("TypeIdent").appliedTo(t.Lit(name))
    case m.Pat.Wildcard() =>
      selectToolbox("Wildcard").appliedTo()
    case m.Pat.Bind(lhs, rhs) =>
      selectToolbox("Bind").appliedTo(lift(lhs), lift(rhs))
    case m.Pat.Alternative(lhs, rhs) =>
      selectToolbox("Alternative").appliedTo(lift(lhs), lift(rhs))
    case m.Pat.Tuple(args) =>
      selectToolbox("Tuple").appliedTo(liftSeq(args))
    case m.Pat.Extract(ref, targs, args) =>
      if (targs.size == 0)
        selectToolbox("Apply").appliedTo(lift(ref), liftSeq(args))
      else
        selectToolbox("Apply").appliedTo(
          selectToolbox("TypeApply").appliedTo(lift(ref), liftSeq(targs)),
          liftSeq(args)
        )
    case m.Pat.ExtractInfix(lhs, m.Term.Name(op), rhs) =>
      selectToolbox("Infix").appliedTo(lift(lhs), t.Lit(op), liftSeq(rhs))
    case m.Pat.Interpolate(m.Term.Name(tag), parts, args) =>
      selectToolbox("Interpolate").appliedTo(t.Lit(tag), liftSeq(parts), liftSeq(args))
    // case m.Pat.Xml(parts, args) =>
    case m.Pat.Typed(lhs, rhs) =>
      selectToolbox("Ascribe").appliedTo(lift(lhs), lift(rhs))
    // case m.Pat.Arg.SeqWildcard() =>
    case m.Pat.Type.Wildcard() =>
      selectToolbox("TypeWildcard").appliedTo()
    // case m.Pat.Type.Project(qual, name) =>
    case m.Pat.Type.Apply(tpe, args) =>
      selectToolbox("TypeApply").appliedTo(lift(tpe), liftSeq(args))
    case m.Pat.Type.ApplyInfix(lhs, op, rhs) =>
      selectToolbox("TypeApplyInfix").appliedTo(lift(lhs), lift(op), lift(rhs))
    case m.Pat.Type.Function(params, res) =>
      selectToolbox("TypeFunction").appliedTo(liftSeq(params), lift(res))
    case m.Pat.Type.Tuple(args) =>
      selectToolbox("TypeTuple").appliedTo(liftSeq(args))
    // case m.Pat.Type.With(lhs, rhs) =>
    case m.Pat.Type.And(lhs, rhs) =>
      selectToolbox("TypeAnd").appliedTo(lift(lhs), lift(rhs))
    case m.Pat.Type.Or(lhs, rhs) =>
      selectToolbox("TypeOr").appliedTo(lift(lhs), lift(rhs))
    case m.Pat.Type.Refine(tpe, stats) =>
      selectToolbox("TypeRefine").appliedTo(liftOpt(tpe), liftSeq(stats))
    // case m.Pat.Type.Existential(tpe, stats) =>
    case m.Pat.Type.Annotate(tpe, annots) =>
      selectToolbox("TypeAnnotated").appliedTo(lift(tpe), liftSeq(annots))
    case m.Pat.Type.Placeholder(bounds) =>
      selectToolbox("TypeWildcard").appliedTo(lift(bounds))

    case m.Decl.Val(mods, pats, tpe) =>
      require(pats.size > 0)
      val modifiers = t.Infix(liftSeq(mods), ":+", selectToolbox("Mod.Val").appliedTo())
      liftValDef(modifiers, pats, scalaSome.appliedTo(lift(tpe)), scalaNone)
    case m.Decl.Var(mods, pats, tpe) =>
      require(pats.size > 0)
      val modifiers = t.Infix(liftSeq(mods), ":+", selectToolbox("Mod.Var").appliedTo())
      liftValDef(modifiers, pats, scalaSome.appliedTo(lift(tpe)), scalaNone)
    case m.Decl.Def(mods, name, tparams, paramss, tpe) =>
      selectToolbox("scala.meta.Decl.Def").appliedTo(liftSeq(mods), lift(name), liftSeq(tparams), liftSeqSeq(paramss), lift(tpe))
    case m.Decl.Type(mods, name, tparams, bounds) =>
      selectToolbox("scala.meta.Decl.Type").appliedTo(liftSeq(mods), lift(name), liftSeq(tparams), lift(bounds))

    case m.Defn.Val(mods, pats, tpe, rhs) =>
      require(pats.size > 0)
      val modifiers = t.Infix(liftSeq(mods), ":+", selectToolbox("Mod.Val").appliedTo())
      liftValDef(modifiers, pats, liftOpt(tpe), lift(rhs))
    case m.Defn.Var(mods, pats, tpe, rhs) =>
      require(pats.size > 0)
      val modifiers = t.Infix(liftSeq(mods), ":+",  selectToolbox("Mod.Var").appliedTo())
      liftValDef(modifiers, pats, liftOpt(tpe), liftOpt(rhs))
    case m.Defn.Def(mods, name, tparams, paramss, tpe, body) =>
      selectToolbox("DefDef").appliedTo(liftSeq(mods), liftName(name), liftSeq(tparams), liftSeqSeq(paramss), liftOpt(tpe), lift(body))
    // case m.Defn.Macro(mods, name, tparams, paramss, tpe, body) =>
    case m.Defn.Type(mods, name, tparams, body) =>
      selectToolbox("Type").appliedTo(liftSeq(mods), liftName(name), liftSeq(tparams), lift(body))
    case m.Defn.Class(mods, name, tparams, ctor, m.Template(_, parents, self, stats)) =>
      selectToolbox("Class").appliedTo(
        liftSeq(mods),
        liftName(name),
        liftSeq(tparams),
        scalaSome.appliedTo(lift(ctor)),
        liftSeqTrees(parents.map(liftInitCall)),
        liftSelf(self),
        liftOptSeq(stats)
      )
    case m.Defn.Trait(mods, name, tparams, ctor, m.Template(_, parents, self, stats)) =>
      selectToolbox("Trait").appliedTo(
        liftSeq(mods),
        liftName(name),
        liftSeq(tparams),
        scalaSome.appliedTo(lift(ctor)),
        liftSeqTrees(parents.map(liftInitCall)),
        liftSelf(self),
        liftOptSeq(stats)
      )
    case m.Defn.Object(mods, name, m.Template(_, parents, self, stats)) =>
      selectToolbox("Object").appliedTo(
        liftSeq(mods),
        liftName(name),
        liftSeqTrees(parents.map(liftInitCall)),
        liftSelf(self),
        liftOptSeq(stats)
      )

    // case m.Pkg(ref, stats) =>
    // case m.Pkg.Object(mods, name, templ) =>

    case m.Ctor.Primary(mods, name, paramss) =>
      selectToolbox("PrimaryCtor").appliedTo(liftSeq(mods), liftSeqSeq(paramss))
    case m.Ctor.Secondary(mods, name, paramss, body) =>
      selectToolbox("SecondaryCtor").appliedTo(liftSeq(mods), liftSeqSeq(paramss), lift(body))
    // case m.Ctor.Ref.Name(v) =>                       // handled by liftInitCall
    // case m.Ctor.Ref.Select(qual, name) =>
    // case m.Ctor.Ref.Project(qual, name) =>
    // case m.Ctor.Ref.Function(name) =>                // TODO: what's this?

    // case m.Template(early, parents, self, stats) =>

    case m.Mod.Annot(body) =>
      selectToolbox("Mod.Annot").appliedTo(lift(body))
    case m.Mod.Private(within) =>
      selectToolbox("Mod.Private").appliedTo(lift(within))
    case m.Mod.Protected(within) =>
      selectToolbox("Mod.Protected").appliedTo(lift(within))
    case m.Mod.Implicit() =>
      selectToolbox("Mod.Implicit").appliedTo()
    case m.Mod.Final() =>
      selectToolbox("Mod.Final").appliedTo()
    case m.Mod.Sealed() =>
      selectToolbox("Mod.Sealed").appliedTo()
    case m.Mod.Override() =>
      selectToolbox("Mod.Override").appliedTo()
    case m.Mod.Case() =>
      selectToolbox("Mod.Case").appliedTo()
    case m.Mod.Abstract() =>
      selectToolbox("Mod.Abstract").appliedTo()
    case m.Mod.Covariant() =>
      selectToolbox("Mod.Covariant").appliedTo()
    case m.Mod.Contravariant() =>
      selectToolbox("Mod.Contravariant").appliedTo()
    case m.Mod.Lazy() =>
      selectToolbox("Mod.Lazy").appliedTo()
    case m.Mod.ValParam() =>
      selectToolbox("Mod.Val").appliedTo()
    case m.Mod.VarParam() =>
      selectToolbox("Mod.Var").appliedTo()
    case m.Mod.Inline() =>
      selectToolbox("Mod.Inline").appliedTo()

    case m.Enumerator.Generator(pat, rhs) =>
      selectToolbox("GenFrom").appliedTo(lift(pat), lift(rhs))
    case m.Enumerator.Val(pat, rhs) =>
      selectToolbox("GenAlias").appliedTo(lift(pat), lift(rhs))
    case m.Enumerator.Guard(cond) =>
      selectToolbox("Guard").appliedTo(lift(cond))

    case m.Import(importers) =>
      selectToolbox("Import").appliedTo(liftSeq(importers))
    case m.Importer(ref, importees) =>
      selectToolbox("ImportItem").appliedTo(lift(ref), liftSeq(importees))
    case m.Importee.Wildcard() =>
      selectToolbox("ImportName").appliedTo(t.Lit("_"))
    case m.Importee.Name(m.Name.Indeterminate(name)) =>
      selectToolbox("ImportName").appliedTo(t.Lit(name))
    case m.Importee.Rename(m.Name.Indeterminate(name), m.Name.Indeterminate(rename)) =>
      selectToolbox("ImportRename").appliedTo(t.Lit(name), t.Lit(rename))
    case m.Importee.Unimport(m.Name.Indeterminate(name)) =>
      selectToolbox("ImportHide").appliedTo(t.Lit(name))

    case m.Case(pat, cond, body) =>
      selectToolbox("Case").appliedTo(lift(pat), liftOpt(cond), lift(body))

    // case m.Source(stats) =>
    //  select("scala.meta.Source").appliedTo(liftSeq(stats))
  }
}
