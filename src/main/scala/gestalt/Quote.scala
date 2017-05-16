package scala.gestalt

import scala.collection.immutable.Seq
import scala.{meta => m}
import scala.compat.Platform.EOL

/** Lift scala.meta trees as t.trees */
abstract class Quote(val t: Toolbox, val toolboxName: String) {
  import Quasiquote.Hole
  import t._

  type Quasi = m.internal.ast.Quasi

  // fields
  // args: List[t.Tree[Any]]
  def args: List[t.Tree]
  def isTerm: Boolean
  def enclosingTree: t.Tree

  // helpers
  private implicit class TreeOps(val tree: t.TermTree) {
    def select(name: String): t.TermTree = t.Select(tree, name)

    def appliedTo(args: t.TermTree*): t.TermTree = t.Apply(tree, args.toList)
  }

  // lifted trees
  lazy val scalaNil  = select("scala.Nil")
  lazy val scalaList = select("scala.List")
  lazy val scalaSome = select("scala.Some")
  lazy val scalaNone = select("scala.None")
  lazy val toolbox   = t.Ident(toolboxName)
  lazy val root      = t.Ident("_root_")

  private def select(path: String): t.TermTree = {
    val parts = path.split('.')

    parts.foldLeft[t.TermTree](root) { (prefix, name) =>
      prefix.select(name)
    }
  }

  private def selectToolbox(path: String): t.TermTree = {
    val parts = path.split('.')

    val qual = parts.init.foldLeft[t.TermTree](toolbox) { (prefix, name) =>
      prefix.select(name)
    }

    t.Select(qual, parts.last)
  }

  /** {{{
    * AnyTree = t.Tree | t.TypeTree
    * (trees: Seq[m.Tree[A]]) => (t.Tree[Seq[String]] | t.Tree[Seq[AnyTree[?]]])
    * }}}*/
  def liftSeq(trees: Seq[m.Tree]): t.TermTree =  {
    def loop(trees: List[m.Tree], acc: Option[t.TermTree], prefix: List[m.Tree]): t.TermTree = trees match {
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
            t.error(m.internal.parsers.Messages.QuasiquoteAdjacentEllipsesInPattern(quasi.rank), enclosingTree.pos)
            t.Lit(null)
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

  /** {{{
    * (treess: Seq[Seq[m.Tree[A]]]) => t.Tree[Seq[Seq[t.Tree[A]]]]
    * }}}*/
  def liftSeqSeq(treess: Seq[Seq[m.Tree]]): t.TermTree = {
    val tripleDotQuasis = treess.flatten.collect{ case quasi: Quasi if quasi.rank == 2 => quasi }
    if (tripleDotQuasis.length == 0) {
      val args = treess.map(liftSeq)
      scalaList.appliedTo(args: _*)
    } else if (tripleDotQuasis.length == 1) {
      if (treess.flatten.length == 1) liftQuasi(tripleDotQuasis(0))
      else {
        t.error("implementation restriction: can't mix ...$ with anything else in parameter lists." +
          EOL + "See https://github.com/scalameta/scalameta/issues/406 for details.", enclosingTree.pos)
        t.Lit(null)
      }
    } else {
      t.error(m.internal.parsers.Messages.QuasiquoteAdjacentEllipsesInPattern(2), enclosingTree.pos)
      t.Lit(null)
    }
  }

  /** (trees: Option[m.Tree[A]]) => t.Tree[Option[t.Tree[A]]] */
  def liftOpt(treeOpt: Option[m.Tree]): t.TermTree = treeOpt match {
    case Some(quasi: Quasi) =>
      liftQuasi(quasi, optional = true)
    case Some(tree) =>
      scalaSome.appliedTo(lift(tree))
    case None =>
      scalaNone
  }

  /**{{{(treesOpt: Option[Seq[m.Tree[A]]]) => t.Tree[Seq[t.Tree[A]}}} */
  def liftOptSeq(treesOpt: Option[Seq[m.Tree]]): t.TermTree = treesOpt match {
    case Some(Seq(quasi: Quasi)) if quasi.rank > 0 && !isTerm =>
      liftQuasi(quasi)
    case Some(trees) =>
      liftSeq(trees)
    case None =>
      scalaNil
  }

  /** {{{(tree: Quasi, Int, Boolean) => ( t.Tree[Any]| t.Tree[t.Tree[?]])}}} */
  def liftQuasi(quasi: Quasi, expectedRank: Int = 0, optional: Boolean = false): t.TermTree = {
    if (quasi.rank > 0) return liftQuasi(quasi.tree.asInstanceOf[Quasi], quasi.rank, optional)

    def arg(i: Int) =
      if (optional) {
        scalaSome.appliedTo(args(i).asInstanceOf[t.TermTree])
      } else {
        args(i).asInstanceOf[t.TermTree]
      }

    quasi.tree match {
      case m.Term.Name(Hole(i)) => arg(i)
      case m.Type.Name(Hole(i)) => arg(i)
    }
  }

  private object TypeArguments {
    def unapply(tree: m.Tree) = tree match {
      case m.Term.ApplyType(inner, targs) => Some(inner -> liftSeq(targs))
      case inner => Some(inner -> liftSeq(Nil))
    }
  }

  private object Qualifier {
    def unapply(tree: m.Tree) = tree match {
      case m.Ctor.Ref.Select(qual, inner) => Some(inner -> scalaSome.appliedTo(lift(qual)))
      case inner => Some(inner -> scalaNone)
    }
  }

  private object Argss {
    def unapply(tree: m.Tree) = tree match {
      case m.internal.ast.Helpers.TermApply(inner, argss) => Some(inner -> argss)
      case inner => Some(inner -> Nil)
    }
  }

  /** Lift initcall : {{{qual.T[A, B](x, y)(z)}}}
    * {{{(tree: m.Tree[A]) => t.Tree[t.Tree[A]]}}} */
  def liftInitCall(tree: m.Tree): t.TermTree = {
    val Argss(TypeArguments(Qualifier(m.Ctor.Ref.Name(name), qualOpt), targs), argss) = tree
    selectToolbox("InitCall").appliedTo(qualOpt, t.Lit(name), targs, liftSeqSeq(argss))
  }

  def liftNewInstance(tree: m.Tree): t.TermTree = {
    val Argss(TypeArguments(Qualifier(m.Ctor.Ref.Name(name), qualOpt), targs), argss) = tree
    selectToolbox("NewInstance").appliedTo(qualOpt, t.Lit(name), targs, liftSeqSeq(argss))
  }

  /** {{{(trees: Seq[t.Tree[t.Tree[A]]]) => t.Tree[Seq[t.Tree[A]]]}}} */
  def liftSeqTrees(trees: Seq[t.TermTree]): t.TermTree = trees match {
    case head :: rest =>
      // Infix is sugar-less
      // the List is constructed as Nil.::(c).::(b).::(a)
      // Not a :: b :: c :: Nil
      t.Infix(head, "::", liftSeqTrees(rest))
    case _ =>
      scalaNil
  }

  /**
    * {{{
    * (mods: t.Tree[t.Mods],
    *  pats: Seq[m.Pat],
    *  tpe: t.Tree[Option[t.TypeTree[B]]],
    *  rhs: t.Tree[Option[t.Tree[C]]],
    *  name: String) => t.Tree[t.Tree[?]]
    * }}}
    *
    * */
  def liftValDef(mods: t.TermTree, pats: Seq[m.Pat], tpe: t.TermTree, rhs: t.TermTree, isDecl: Boolean): t.TermTree = {
    if (pats.size == 1) {
      // AnyTree = t.Tree | t.TypeTree
      //left: t.Tree[AnyTree[?]] | t.Tree[String]
      val left = pats(0) match {
        case quasi: Quasi =>
          liftQuasi(quasi)
        case m.Term.Name(name) =>
          t.Lit(name)
        case pat =>
          lift(pat)
      }

      if (isDecl)
        selectToolbox("ValDecl").appliedTo(mods, left, tpe)
      else
        selectToolbox("ValDef").appliedTo(mods, left, tpe, rhs)
    }
    else {
      if (isDecl)
        selectToolbox("SeqDecl").appliedTo(mods, liftSeq(pats), tpe)
      else
        selectToolbox("SeqDef").appliedTo(mods, liftSeq(pats), tpe, rhs)
    }
  }

  /** Lift self annotation in class definition
    * {{{(tree: m.Tree[A]) => t.Tree[Option[t.Tree[A]]]}}} */
  def liftSelf(tree: m.Tree): t.TermTree = tree match {
    case m.Term.Param(_, m.Name.Anonymous(), _, _) =>
      scalaNone
    case m.Term.Param(_, m.Term.Name(name), Some(tp), _) =>
      scalaSome appliedTo selectToolbox("Self").appliedTo(t.Lit(name), lift(tp))
    case m.Term.Param(_, m.Term.Name(name), _, _) =>
      scalaSome appliedTo selectToolbox("Self").appliedTo(t.Lit(name))
  }

  /** lift name to either Lit or Quasi
    * {{{(name: m.Name) => t.Tree[String]}} */
  def liftName(name: m.Name): t.TermTree = name match {
    case quasi: Quasi => liftQuasi(quasi)
    case _ => t.Lit(name.value)
  }

  /** lift modifiers */
  def liftMods(mods: Seq[m.Tree]): t.TermTree = {
    mods match {
      case Seq(quasi: Quasi) => return liftQuasi(quasi)
      case _ =>
        if (!isTerm) {
          t.error("Match modifiers in syntax is problematic and not supported. Match the modifiers with a variable instead or $_ to ignore them.", enclosingTree.pos)
          return t.Ident("_")
        }
    }

    val zero: t.TermTree = selectToolbox("emptyMods")

    mods.foldLeft(zero) { (acc, mod) =>
      mod match {
        case m.Mod.Annot(body) =>
          t.Select(acc, "withAddedAnnotation").appliedTo(lift(body))
        case m.Mod.Private(within) =>
          val scope = within match {
            case m.Name.Indeterminate(name)      => name
            case m.Term.This(m.Name.Anonymous()) => "this"
            case m.Name.Anonymous()              => ""
          }
          t.Select(acc, "setPrivate").appliedTo(t.Lit(scope))
        case m.Mod.Protected(within) =>
          val scope = within match {
            case m.Name.Indeterminate(name)      => name
            case m.Term.This(m.Name.Anonymous()) => "this"
            case m.Name.Anonymous()              => ""
          }
          t.Select(acc, "setProtected").appliedTo(t.Lit(scope))
        case m.Mod.Implicit() =>
          t.Select(acc, "setImplicit")
        case m.Mod.Final() =>
          t.Select(acc, "setFinal")
        case m.Mod.Sealed() =>
          t.Select(acc, "setSealed")
        case m.Mod.Override() =>
          t.Select(acc, "setOverride")
        case m.Mod.Case() =>
          t.Select(acc, "setCase")
        case m.Mod.Abstract() =>
          t.Select(acc, "setAbstract")
        case m.Mod.Covariant() =>
          t.Select(acc, "setCovariant")
        case m.Mod.Contravariant() =>
          t.Select(acc, "setContravariant")
        case m.Mod.Lazy() =>
          t.Select(acc, "setLazy")
        case m.Mod.ValParam() =>
          t.Select(acc, "setValParam")
        case m.Mod.VarParam() =>
          t.Select(acc, "setVarParam")
        case m.Mod.Inline() =>
          t.Select(acc, "setInline")
      }
    }
  }

  /** {{{
    * AnyTree = t.Tree | t.TypeTree
    * (tree: m.Tree[A]) => t.Tree[AnyTree[A]]
    * }}} */
  def lift(tree: m.Tree): t.TermTree = tree match {
    case quasi: Quasi  =>
      liftQuasi(quasi)

    case m.Lit(value) =>
      selectToolbox("Lit").appliedTo(t.Lit(value))

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
    case m.Term.Select(qual, quasi: Quasi) =>
      selectToolbox("Select").appliedTo(lift(qual), liftQuasi(quasi))
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
      selectToolbox("Update").appliedTo(lift(fun), liftSeqSeq(argss), lift(rhs))
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
      selectToolbox("If").appliedTo(lift(cond), lift(thenp), scalaSome.appliedTo(lift(elsep)))
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
      selectToolbox("For.ForDo").appliedTo(liftSeq(enums), lift(body))
    case m.Term.ForYield(enums, body) =>
      selectToolbox("For.ForYield").appliedTo(liftSeq(enums), lift(body))
    case m.Term.New(m.Template(Nil, Seq(mctor), m.Term.Param(Nil, m.Name.Anonymous(), None, None), None)) =>
      liftNewInstance(mctor)
    case m.Term.New(m.Template(_, parents, self, stats)) =>
      // parentCalls: t.Tree[Seq[t.Tree[B]]]
      val parentCalls = liftSeqTrees(parents.map(liftInitCall))
      // anonym: t.Tree[t.Tree[C]]
      selectToolbox("NewAnonymClass").appliedTo(parentCalls, liftSelf(self), liftOptSeq(stats))
    case m.Term.Placeholder() =>
      if (isTerm) t.error("placeholder is not supported", enclosingTree.pos)
      selectToolbox("Ident").appliedTo(t.Lit("_"))
    case m.Term.Eta(expr) =>
      selectToolbox("Postfix").appliedTo(lift(expr), t.Lit("_"))
    case m.Term.Arg.Named(m.Term.Name(name), expr) =>
      selectToolbox("Named").appliedTo(t.Lit(name), lift(expr))
    case m.Term.Arg.Repeated(expr) =>
      selectToolbox("Repeated").appliedTo(lift(expr))
    case m.Term.Param(mods, m.Term.Name(name), tpe, default) =>
      selectToolbox("Param").appliedTo(liftMods(mods), t.Lit(name), liftOpt(tpe), liftOpt(default))

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
      selectToolbox("TypeFunction").appliedTo(liftSeq(params), lift(res))
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
      selectToolbox("TypeWildcard").appliedTo(lift(bounds)) // FIXME TypeWildcard is not defined in toolbox
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
        liftMods(mods), t.Lit(nameStr), liftSeq(tparams), lift(tbounds), liftSeq(cbounds)
      )

    // patterns
    case m.Pat.Var.Term(m.Term.Name(name)) =>
      selectToolbox("Ident").appliedTo(t.Lit(name))
    case m.Pat.Var.Type(m.Type.Name(name)) =>
      selectToolbox("TypeIdent").appliedTo(t.Lit(name))
    case m.Pat.Wildcard() =>
      selectToolbox("Wildcard").appliedTo() // FIXME Wildcard is not defined in toolbox
    case m.Pat.Bind(m.Pat.Var.Term(name), expr) =>
      selectToolbox("Bind").appliedTo(t.Lit(name), lift(expr))
    case m.Pat.Alternative(lhs, rhs) =>
      selectToolbox("Alternative").appliedTo(scalaList.appliedTo(lift(lhs), lift(rhs)))
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
      selectToolbox("TypeWildcard").appliedTo() // FIXME TypeWildcard is not defined in toolbox
    // case m.Pat.Type.Project(qual, name) =>
    case m.Pat.Type.Apply(tpe, args) =>
      selectToolbox("TypeApply").appliedTo(lift(tpe), liftSeq(args))
    case m.Pat.Type.ApplyInfix(lhs, op, rhs) =>
      selectToolbox("TypeApplyInfix").appliedTo(lift(lhs), liftName(op), lift(rhs))
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
      selectToolbox("TypeWildcard").appliedTo(lift(bounds)) // FIXME TypeWildcard is not defined in toolbox

    case m.Decl.Val(mods, pats, tpe) =>
      require(pats.size > 0)
      val modifiers = liftMods(mods)
      liftValDef(modifiers, pats, lift(tpe), null, isDecl = true)
    case m.Decl.Var(mods, pats, tpe) =>
      require(pats.size > 0)
      val modifiers = t.Select(liftMods(mods), "setMutable")
      liftValDef(modifiers, pats, lift(tpe), null, isDecl = true)
    case m.Decl.Def(mods, name, tparams, paramss, tpe) =>
      selectToolbox("DefDecl").appliedTo(liftMods(mods), liftName(name), liftSeq(tparams), liftSeqSeq(paramss), lift(tpe))
    case m.Decl.Type(mods, name, tparams, bounds) =>
      selectToolbox("TypeDecl").appliedTo(liftMods(mods), liftName(name), liftSeq(tparams), scalaSome.appliedTo(lift(bounds)))

    case m.Defn.Val(mods, pats, tpe, rhs) =>
      require(pats.size > 0)
      val modifiers = liftMods(mods)
      liftValDef(modifiers, pats, liftOpt(tpe), lift(rhs), isDecl = false)
    case m.Defn.Var(mods, pats, tpe, rhs) =>
      require(pats.size > 0)
      val modifiers = t.Select(liftMods(mods), "setMutable")
      liftValDef(modifiers, pats, liftOpt(tpe), liftOpt(rhs), isDecl = false)
    case m.Defn.Def(mods, name, tparams, paramss, tpe, body) =>
      selectToolbox("DefDef").appliedTo(liftMods(mods), liftName(name), liftSeq(tparams), liftSeqSeq(paramss), liftOpt(tpe), lift(body))
    // case m.Defn.Macro(mods, name, tparams, paramss, tpe, body) =>
    case m.Defn.Type(mods, name, tparams, body) =>
      selectToolbox("TypeAlias").appliedTo(liftMods(mods), liftName(name), liftSeq(tparams), lift(body))
    case m.Defn.Class(mods, name, tparams, ctor, m.Template(_, parents, self, stats)) =>
      val (cmods, paramss) = ctor match {
        case m.Ctor.Primary(mods, name, paramss) =>
          (mods, paramss)
      }
      selectToolbox("Class").appliedTo(
        liftMods(mods),
        liftName(name),
        liftSeq(tparams),
        liftMods(cmods),
        liftSeqSeq(paramss),
        liftSeqTrees(parents.map(liftInitCall)),
        liftSelf(self),
        liftOptSeq(stats)
      )
    case m.Defn.Trait(mods, name, tparams, ctor, m.Template(_, parents, self, stats)) =>
       val (cmods, paramss) = ctor match {
        case m.Ctor.Primary(mods, name, paramss) =>
          (mods, paramss)
      }
      selectToolbox("Trait").appliedTo(
        liftMods(mods),
        liftName(name),
        liftSeq(tparams),
        liftMods(cmods),
        liftSeqSeq(paramss),
        liftSeqTrees(parents.map(liftInitCall)),
        liftSelf(self),
        liftOptSeq(stats)
      )
    case m.Defn.Object(mods, name, m.Template(_, parents, self, stats)) =>
      selectToolbox("Object").appliedTo(
        liftMods(mods),
        liftName(name),
        liftSeqTrees(parents.map(liftInitCall)),
        liftSelf(self),
        liftOptSeq(stats)
      )

    // case m.Pkg(ref, stats) =>
    // case m.Pkg.Object(mods, name, templ) =>

    case m.Ctor.Secondary(mods, name, paramss, body) =>
      selectToolbox("SecondaryCtor").appliedTo(liftMods(mods), liftSeqSeq(paramss), lift(body))
    // case m.Ctor.Ref.Name(v) =>                       // handled by liftInitCall
    // case m.Ctor.Ref.Select(qual, name) =>
    // case m.Ctor.Ref.Project(qual, name) =>
    // case m.Ctor.Ref.Function(name) =>                // TODO: what's this?

    // case m.Template(early, parents, self, stats) =>


    case m.Enumerator.Generator(pat, rhs) =>
      selectToolbox("For.GenFrom").appliedTo(lift(pat), lift(rhs))
    case m.Enumerator.Val(pat, rhs) =>
      selectToolbox("For.GenAlias").appliedTo(lift(pat), lift(rhs))
    case m.Enumerator.Guard(cond) =>
      selectToolbox("For.Guard").appliedTo(lift(cond))

    case m.Import(importers) =>
      selectToolbox("Import").appliedTo(liftSeq(importers))
    case m.Importer(ref, importees) =>
      selectToolbox("Import.Item").appliedTo(lift(ref), liftSeq(importees))
    case m.Importee.Wildcard() =>
      selectToolbox("Import.Name").appliedTo(t.Lit("_"))
    case m.Importee.Name(m.Name.Indeterminate(name)) =>
      selectToolbox("Import.Name").appliedTo(t.Lit(name))
    case m.Importee.Rename(m.Name.Indeterminate(name), m.Name.Indeterminate(rename)) =>
      selectToolbox("Import.Rename").appliedTo(t.Lit(name), t.Lit(rename))
    case m.Importee.Unimport(m.Name.Indeterminate(name)) =>
      selectToolbox("Import.Hide").appliedTo(t.Lit(name))

    case m.Case(pat, cond, body) =>
      selectToolbox("Case").appliedTo(lift(pat), liftOpt(cond), lift(body))

    // case m.Source(stats) =>
    //  select("scala.meta.Source").appliedTo(liftSeq(stats))
  }
}
