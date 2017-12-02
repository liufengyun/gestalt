package scala.gestalt.quasiquotes

import scala.collection.immutable.Seq
import scala.{meta => m}
import scala.compat.Platform.EOL
import scala.gestalt.api._
import untpd._
import Term.{ Ident, Select, Infix }
import tpd.{ Lit }
import scala.gestalt.options.unsafe

/** Lift scala.meta trees as trees */
class Quote(args: List[Tree], isTerm: Boolean, enclosingTree: Tree) {
  type Quasi = m.internal.ast.Quasi

  // lifted trees
  lazy val scalaNil       = root.select("scala.Nil")
  lazy val scalaList      = root.select("scala.List")
  lazy val scalaSome      = root.select("scala.Some")
  lazy val scalaNone      = root.select("scala.None")

  private def untpd(path: String): TermTree = Path("scala.gestalt.api.untpd." + path)

  private def Path(path: String): TermTree = {
    val parts = path.split('.')

    parts.tail.foldLeft[TermTree](Ident.apply(parts.head)) { (prefix, name) =>
      prefix.select(name)
    }
  }

  /** {{{
    * AnyTree = Tree | TypeTree
    * (trees: Seq[m.Tree[A]]) => (Tree[Seq[String]] | Tree[Seq[AnyTree[?]]])
    * }}}*/
  def liftSeq(trees: Seq[m.Tree]): TermTree =  {
    def loop(trees: List[m.Tree], acc: Option[TermTree], prefix: List[m.Tree]): TermTree = trees match {
      case (quasi: Quasi) +: rest if quasi.rank == 1 =>
        if (acc.isEmpty) {
          if (prefix.isEmpty) loop(rest, Some(liftQuasi(quasi)), Nil)
          else loop(rest, Some(prefix.foldRight(liftQuasi(quasi))((curr, acc) => {
            val currElement = lift(curr)
            Infix(currElement, "+:", acc)
          })), Nil)
        } else {
          require(prefix.isEmpty)
          if (isTerm) loop(rest, Some(Infix(acc.get, "++", liftQuasi(quasi))), Nil)
          else {
            error(m.internal.parsers.Messages.QuasiquoteAdjacentEllipsesInPattern(quasi.rank), enclosingTree.pos)
            Lit(null)
          }
        }
      case other +: rest =>
        if (acc.isEmpty) loop(rest, acc, prefix :+ other)
        else {
          require(prefix.isEmpty)
          loop(rest, Some(Infix(acc.get, ":+", lift(other))), Nil)
        }
      case Nil =>
        if (acc.isEmpty)
          scalaList.appliedTo(prefix.map(lift): _*)
        else acc.get
    }

    loop (trees.toList, None, Nil)
  }

  /** {{{
    * (treess: Seq[Seq[m.Tree[A]]]) => Tree[Seq[Seq[Tree[A]]]]
    * }}}*/
  def liftSeqSeq(treess: Seq[Seq[m.Tree]]): TermTree = {
    val tripleDotQuasis = treess.flatten.collect{ case quasi: Quasi if quasi.rank == 2 => quasi }
    if (tripleDotQuasis.length == 0) {
      val args = treess.map(liftSeq)
      scalaList.appliedTo(args: _*)
    } else if (tripleDotQuasis.length == 1) {
      if (treess.flatten.length == 1) liftQuasi(tripleDotQuasis(0))
      else {
        error("implementation restriction: can't mix ...$ with anything else in parameter lists." +
          EOL + "See https://github.com/scalameta/scalameta/issues/406 for details.", enclosingTree.pos)
        Lit(null)
      }
    } else {
      error(m.internal.parsers.Messages.QuasiquoteAdjacentEllipsesInPattern(2), enclosingTree.pos)
      Lit(null)
    }
  }

  /** (trees: Option[m.Tree[A]]) => Tree[Option[Tree[A]]] */
  def liftOpt(treeOpt: Option[m.Tree]): TermTree = treeOpt match {
    case Some(quasi: Quasi) =>
      liftQuasi(quasi, optional = true)
    case Some(tree) =>
      scalaSome.appliedTo(lift(tree))
    case None =>
      scalaNone
  }

  /**{{{(treesOpt: Option[Seq[m.Tree[A]]]) => Tree[Seq[Tree[A]}}} */
  def liftOptSeq(treesOpt: Option[Seq[m.Tree]]): TermTree = treesOpt match {
    case Some(Seq(quasi: Quasi)) if quasi.rank > 0 && !isTerm =>
      liftQuasi(quasi)
    case Some(trees) =>
      liftSeq(trees)
    case None =>
      scalaNil
  }

  /** {{{(tree: Quasi, Int, Boolean) => ( Tree[Any]| Tree[Tree[?]])}}} */
  def liftQuasi(quasi: Quasi, expectedRank: Int = 0, optional: Boolean = false): TermTree = {
    if (quasi.rank > 0) return liftQuasi(quasi.tree.asInstanceOf[Quasi], quasi.rank, optional)

    def arg(i: Int) =
      if (optional) {
        scalaSome.appliedTo(args(i).asInstanceOf[TermTree])
      } else {
        args(i).asInstanceOf[TermTree]
      }

    quasi.tree match {
      case m.Term.Name(Hole(i)) => arg(i)
      case m.Type.Name(Hole(i)) => arg(i)
    }
  }

  private object TypeArguments {
    def unapply(tree: m.Tree) = tree match {
      case m.Term.ApplyType(inner, targs) => Some(inner -> targs)
      case inner => Some(inner -> Nil)
    }
  }

  private object Qualifier {
    def unapply(tree: m.Tree) = tree match {
      case m.Ctor.Ref.Select(qual, inner) => Some(inner -> Some(qual))
      case inner => Some(inner -> None)
    }
  }

  private object Argss {
    def unapply(tree: m.Tree) = tree match {
      case m.internal.ast.Helpers.TermApply(inner, argss) => Some(inner -> argss)
      case inner => Some(inner -> Nil)
    }
  }

  private def composeType(qualOpt: Option[m.Tree], name: String, targs: Seq[m.Tree]): TermTree = qualOpt match {
    case Some(qual) =>
      val pre = untpd("Type.Select").appliedTo(lift(qual), Lit(name))
      if (targs.isEmpty) pre
      else untpd("Type.Apply").appliedTo(pre, liftSeq(targs))
    case None       =>
      val pre = untpd("Type.Ident").appliedTo(Lit(name))
      if (targs.isEmpty) pre
      else untpd("Type.Apply").appliedTo(pre, liftSeq(targs))
  }

  /** Lift initcall : {{{qual.T[A, B](x, y)(z)}}}
    * {{{(tree: m.Tree[A]) => Tree[Tree[A]]}}} */
  def liftInitCall(tree: m.Tree): TermTree = {
    val Argss(TypeArguments(Qualifier(m.Ctor.Ref.Name(name), qualOpt), targs), argss) = tree
    val tp = composeType(qualOpt, name, targs)
    untpd("Defn.InitCall").appliedTo(tp, liftSeqSeq(argss))
  }

  def liftNewInstance(tree: m.Tree): TermTree = {
    val Argss(TypeArguments(Qualifier(m.Ctor.Ref.Name(name), qualOpt), targs), argss) = tree
    val tp = composeType(qualOpt, name, targs)
    untpd("Term.NewInstance").appliedTo(tp, liftSeqSeq(argss))
  }

  /** {{{(trees: Seq[Tree[Tree[A]]]) => Tree[Seq[Tree[A]]]}}} */
  def liftSeqTrees(trees: Seq[TermTree]): TermTree = trees match {
    case head :: rest =>
      Infix(head, "::", liftSeqTrees(rest))
    case _ =>
      scalaNil
  }

  /**
    * {{{
    * (mods: Tree[Mods],
    *  pats: Seq[m.Pat],
    *  tpe: Tree[Option[TypeTree[B]]],
    *  rhs: Tree[Option[Tree[C]]],
    *  name: String) => Tree[Tree[?]]
    * }}}
    *
    * */
  def liftValDef(mods: TermTree, pats: Seq[m.Pat], tpe: TermTree, rhs: TermTree, isDecl: Boolean): TermTree = {
    if (pats.size == 1) {
      // AnyTree = Tree | TypeTree
      //left: Tree[AnyTree[?]] | Tree[String]
      var isPatDef = false

      val left: TermTree = pats(0) match {
        case quasi: Quasi =>
          liftQuasi(quasi)
        case m.Term.Name(name) =>
          Lit(name)
        case m.Pat.Var.Term(m.Term.Name(name)) =>
          Lit(name)
        case pat =>
          isPatDef = true
          lift(pat)
      }

      if (isDecl)
        untpd("Defn.ValDecl").appliedTo(mods, left, tpe)
      else if (isPatDef)
        untpd("Defn.PatDef").appliedTo(mods, left, tpe, rhs)
      else
        untpd("Defn.ValDef").appliedTo(mods, left, tpe, rhs)
    }
    else {
      if (isDecl)
        abort("SeqDecl not supported", enclosingTree.pos)
      else {
        val names = pats.flatMap {
          case m.Pat.Var.Term(m.Term.Name(name)) => List(name)
          case _ => Nil
        }

        if (names.length != pats.length)
          error("Patterns not supported in seqence definition", enclosingTree.pos)

        abort("SeqDef not supported", enclosingTree.pos)
      }
    }
  }

  /** Lift self annotation in class definition
    * {{{(tree: m.Tree[A]) => Tree[Option[Tree[A]]]}}} */
  def liftSelf(tree: m.Tree): TermTree = tree match {
    case m.Term.Param(_, m.Name.Anonymous(), _, _) =>
      scalaNone
    case m.Term.Param(_, m.Term.Name(name), Some(tp), _) =>
      scalaSome appliedTo untpd("Defn.Self").appliedTo(Lit(name), lift(tp))
    case m.Term.Param(_, m.Term.Name(name), _, _) =>
      scalaSome appliedTo untpd("Defn.Self").appliedTo(Lit(name))
  }

  /** lift name to either Lit or Quasi
    * {{{(name: m.Name) => Tree[String]}} */
  def liftName(name: m.Name): TermTree = name match {
    case quasi: Quasi => liftQuasi(quasi)
    case _ => Lit(name.value)
  }

  /** lift modifiers */
  def liftMods(mods: Seq[m.Tree]): TermTree = {
    mods match {
      case Seq(quasi: Quasi) => return liftQuasi(quasi)
      case _ =>
        if (!isTerm) {
          error("Match modifiers in syntax is problematic and not supported. Match the modifiers with a variable instead or $_ to ignore them.", enclosingTree.pos)
          return untpd("Pat.Var").appliedTo(Lit("_"))
        }
    }

    val zero: TermTree = untpd("emptyMods")

    mods.foldLeft(zero) { (acc, mod) =>
      mod match {
        case m.Mod.Annot(body) =>
          Select(acc, "withAddedAnnotation").appliedTo(lift(body))
        case m.Mod.Private(within) =>
          val scope = within match {
            case m.Name.Indeterminate(name)      => name
            case m.Term.This(m.Name.Anonymous()) => "this"
            case m.Name.Anonymous()              => ""
          }
          Select(acc, "setPrivate").appliedTo(Lit(scope))
        case m.Mod.Protected(within) =>
          val scope = within match {
            case m.Name.Indeterminate(name)      => name
            case m.Term.This(m.Name.Anonymous()) => "this"
            case m.Name.Anonymous()              => ""
          }
          Select(acc, "setProtected").appliedTo(Lit(scope))
        case m.Mod.Implicit() =>
          Select(acc, "setImplicit")
        case m.Mod.Final() =>
          Select(acc, "setFinal")
        case m.Mod.Sealed() =>
          Select(acc, "setSealed")
        case m.Mod.Override() =>
          Select(acc, "setOverride")
        case m.Mod.Case() =>
          Select(acc, "setCase")
        case m.Mod.Abstract() =>
          Select(acc, "setAbstract")
        case m.Mod.Covariant() =>
          Select(acc, "setCovariant")
        case m.Mod.Contravariant() =>
          Select(acc, "setContravariant")
        case m.Mod.Lazy() =>
          Select(acc, "setLazy")
        case m.Mod.ValParam() =>
          Select(acc, "setValParam")
        case m.Mod.VarParam() =>
          Select(acc, "setVarParam")
        case m.Mod.Inline() =>
          Select(acc, "setInline")
      }
    }
  }

  /** {{{
    * AnyTree = Tree | TypeTree
    * (tree: m.Tree[A]) => Tree[AnyTree[A]]
    * }}} */
  def lift(tree: m.Tree): TermTree = tree match {
    case quasi: Quasi  =>
      liftQuasi(quasi)

    case m.Lit(value) =>
      untpd("Term.Lit").appliedTo(Lit(value))

    // case m.Name.Anonymous() =>
    // case m.Name.Indeterminate(name) =>

    case m.Term.This(qual)  =>
      val name = qual match {
        case m.Name.Anonymous() => ""
        case m.Name.Indeterminate(name) => name
      }

      untpd("Term.This").appliedTo(Lit(name))
    case m.Term.Super(thisp, superp) =>
      val qual = thisp match {
        case m.Name.Anonymous() => ""
        case m.Name.Indeterminate(name) => name
      }

      val mix = superp match {
        case m.Name.Anonymous() => ""
        case m.Name.Indeterminate(name) => name
      }
      untpd("Term.Super").appliedTo(Lit(qual), Lit(mix))
    case m.Term.Name(name) =>
      untpd("Term.Ident").appliedTo(Lit(name))
    case m.Term.Select(qual, quasi: Quasi) =>
      untpd("Term.Select").appliedTo(lift(qual), liftQuasi(quasi))
    case m.Term.Select(qual, m.Term.Name(name)) =>
      untpd("Term.Select").appliedTo(lift(qual), Lit(name))
    case m.Term.Interpolate(m.Term.Name(tag), parts, args) =>
      untpd("Term.Interpolate").appliedTo(Lit(tag), liftSeq(parts), liftSeq(args))
    // case m.Term.Xml(parts, args) =>
    case m.Term.Apply(fun, args) =>
      // magic happens here with ...$args
      args match {
        case Seq(quasi: Quasi) if quasi.rank == 2 =>
          untpd("Term.ApplySeq").appliedTo(lift(fun), liftQuasi(quasi))
        case _ =>
          untpd("Term.Apply").appliedTo(lift(fun), liftSeq(args))
      }
    case m.Term.ApplyInfix(lhs, m.Term.Name(name), targs, args) =>
      require(targs.length == 0) // no targs for now
      if (args.size == 1)
        untpd("Term.Infix").appliedTo(lift(lhs), Lit(name), lift(args(0)))
      else
        untpd("Term.Infix").appliedTo(lift(lhs), Lit(name), untpd("Term.Tuple").appliedTo(liftSeq(args)))
    case m.Term.ApplyType(fun, targs) =>
      untpd("Term.ApplyType").appliedTo(lift(fun), liftSeq(targs))
    case m.Term.ApplyUnary(m.Term.Name(op), arg) =>
      untpd("Term.Prefix").appliedTo(Lit(op), lift(arg))
    case m.Term.Assign(lhs, rhs) =>
      untpd("Term.Assign").appliedTo(lift(lhs), lift(rhs))
    case m.Term.Update(fun, argss, rhs) =>
      require(argss.size > 0)
      untpd("Term.Update").appliedTo(lift(fun), liftSeqSeq(argss), lift(rhs))
    case m.Term.Return(expr) =>
      untpd("Term.Return").appliedTo(lift(expr))
    case m.Term.Throw(expr) =>
      untpd("Term.Throw").appliedTo(lift(expr))
    case m.Term.Ascribe(expr, tpe) =>
      untpd("Term.Ascribe").appliedTo(lift(expr), lift(tpe))
    case m.Term.Annotate(expr, annots) =>
      untpd("Term.Annotated").appliedTo(lift(expr), liftSeq(annots))
    case m.Term.Tuple(args) =>
      untpd("Term.Tuple").appliedTo(liftSeq(args))
    case m.Term.Block(stats) =>
      untpd("Term.Block").appliedTo(liftSeq(stats))
    case m.Term.If(cond, thenp, elsep) =>
      untpd("Term.If").appliedTo(lift(cond), lift(thenp), lift(elsep))
    case m.Term.Match(expr, cases) =>
      untpd("Term.Match").appliedTo(lift(expr), liftSeq(cases))
    case m.Term.TryWithCases(expr, catchp, finallyp) =>
      untpd("Term.Try").appliedTo(lift(expr), liftSeq(catchp), liftOpt(finallyp))
    case m.Term.TryWithTerm(expr, catchp, finallyp) =>
      untpd("Term.Try").appliedTo(lift(expr), lift(catchp), liftOpt(finallyp))
    case m.Term.Function(params, body) =>
      untpd("Term.Function").appliedTo(liftSeq(params), lift(body))
    case m.Term.PartialFunction(cases) =>
      untpd("Term.PartialFunction").appliedTo(liftSeq(cases))
    case m.Term.While(expr, body) =>
      untpd("Term.While").appliedTo(lift(expr), lift(body))
    case m.Term.Do(body, expr) =>
      untpd("Term.DoWhile").appliedTo(lift(body), lift(expr))
    case m.Term.For(enums, body) =>
      Path("For.ForDo").appliedTo(liftSeq(enums), lift(body))
    case m.Term.ForYield(enums, body) =>
      Path("For.ForYield").appliedTo(liftSeq(enums), lift(body))
    case m.Term.New(m.Template(Nil, Seq(mctor), m.Term.Param(Nil, m.Name.Anonymous(), None, None), None)) =>
      liftNewInstance(mctor)
    case m.Term.New(m.Template(_, parents, self, stats)) =>
      // parentCalls: Tree[Seq[Tree[B]]]
      val parentCalls = liftSeqTrees(parents.map(liftInitCall))
      // anonym: Tree[Tree[C]]
      untpd("Term.NewAnonymClass").appliedTo(parentCalls, liftSelf(self), liftOptSeq(stats))
    case m.Term.Placeholder() =>
      if (isTerm) error("placeholder is not supported", enclosingTree.pos)
      untpd("Term.Ident").appliedTo(Lit("_"))
    case m.Term.Eta(expr) =>
      untpd("Term.Postfix").appliedTo(lift(expr), Lit("_"))
    case m.Term.Arg.Named(m.Term.Name(name), expr) =>
      untpd("Term.Named").appliedTo(Lit(name), lift(expr))
    case m.Term.Arg.Repeated(expr) =>
      untpd("Term.Repeated").appliedTo(lift(expr))
    case m.Term.Param(mods, m.Term.Name(name), tpe, default) =>
      untpd("Term.Param").appliedTo(liftMods(mods), Lit(name), liftOpt(tpe), liftOpt(default))

    // types
    case m.Type.Name(name) =>
      untpd("Type.Ident").appliedTo(Lit(name))
    case m.Type.Select(qual, m.Type.Name(name)) =>
      untpd("Type.Select").appliedTo(lift(qual), Lit(name))
    // case m.Type.Project(qual, name) =>
    case m.Type.Singleton(ref) =>
      untpd("Type.Singleton").appliedTo(lift(ref))
    case m.Type.Apply(tpe, args) =>
      untpd("Type.Apply").appliedTo(lift(tpe), liftSeq(args))
    case m.Type.ApplyInfix(lhs, m.Type.Name(op), rhs) =>
      untpd("Type.Infix").appliedTo(lift(lhs), Lit(op), lift(rhs))
    case m.Type.Function(params, res) =>
      untpd("Type.Function").appliedTo(liftSeq(params), lift(res))
    case m.Type.Tuple(args) =>
      untpd("Type.Tuple").appliedTo(liftSeq(args))
    // case m.Type.With(lhs, rhs) =>
    case m.Type.And(lhs, rhs) =>
      untpd("Type.And").appliedTo(lift(lhs), lift(rhs))
    case m.Type.Or(lhs, rhs) =>
      untpd("Type.Or").appliedTo(lift(lhs), lift(rhs))
    case m.Type.Refine(tpe, stats) =>
      if (tpe.isEmpty)
        untpd("Type.Refine").appliedTo(liftSeq(stats))
      else
        untpd("Type.Refine").appliedTo(lift(tpe.get), liftSeq(stats))
    // case m.Type.Existential(tpe, stats) =>
    case m.Type.Annotate(tpe, annots) =>
      untpd("Type.Annotated").appliedTo(lift(tpe), liftSeq(annots))
    case m.Type.Placeholder(bounds) =>
      untpd("Defn.TypeParam").appliedTo(Lit("_"), lift(bounds))
    case m.Type.Bounds(lo, hi) =>
      untpd("Type.Bounds").appliedTo(liftOpt(lo), liftOpt(hi))
    case m.Type.Arg.ByName(tpe) =>
      untpd("Type.ByName").appliedTo(lift(tpe))
    case m.Type.Arg.Repeated(tpe) =>
      untpd("Type.Repeated").appliedTo(lift(tpe))
    case m.Type.Param(mods, name, tparams, tbounds, vbounds, cbounds) =>
      require(vbounds.size == 0)
      val nameStr = name match {
        case m.Name.Anonymous() => "_"
        case m.Type.Name(name)  => name
      }

      untpd("Type.Param").appliedTo(
        liftMods(mods), Lit(nameStr), liftSeq(tparams), lift(tbounds), liftSeq(cbounds)
      )

    // patterns
    case m.Pat.Var.Term(m.Term.Name(name)) =>
      untpd("Pat.Var").appliedTo(Lit(name))
    case m.Pat.Var.Type(m.Type.Name(name)) =>
      untpd("Type.Ident").appliedTo(Lit(name))
    case m.Pat.Wildcard() =>
      untpd("Pat.Ident").appliedTo(Lit("_"))
    case m.Pat.Bind(m.Pat.Var.Term(m.Term.Name(name)), expr) =>
      untpd("Pat.Bind").appliedTo(Lit(name), lift(expr))
    case m.Pat.Alternative(lhs, rhs) =>
      untpd("Pat.Alt").appliedTo(scalaList.appliedTo(lift(lhs), lift(rhs)))
    case m.Pat.Tuple(args) =>
      untpd("Pat.Tuple").appliedTo(liftSeq(args))
    case m.Pat.Extract(ref, targs, args) =>
      if (!targs.isEmpty)
        error("Type parameters not supported for extractors", enclosingTree.pos)

      untpd("Pat.Unapply").appliedTo(lift(ref), liftSeq(args))
    case m.Pat.ExtractInfix(lhs, m.Term.Name(op), rhs) =>
      untpd("Pat.Infix").appliedTo(lift(lhs), Lit(op), liftSeq(rhs))
    case m.Pat.Interpolate(m.Term.Name(tag), parts, args) =>
      error("Interpolaters not supported for patterns", enclosingTree.pos)
      untpd("Term.Interpolate").appliedTo(Lit(tag), liftSeq(parts), liftSeq(args))
    // case m.PaXml(parts, args) =>
    case m.Pat.Typed(m.Pat.Var.Term(m.Term.Name(name)), rhs) =>
      untpd("Pat.Ascribe").appliedTo(Lit(name), lift(rhs))
    // case m.PaArg.SeqWildcard() =>
    case m.Pat.Type.Wildcard() =>
      untpd("Type.Ident").appliedTo(Lit("_"))
    // case m.PaType.Project(qual, name) =>
    case m.Pat.Type.Apply(tpe, args) =>
      untpd("Type.Apply").appliedTo(lift(tpe), liftSeq(args))
    case m.Pat.Type.ApplyInfix(lhs, op, rhs) =>
      untpd("Type.Infix").appliedTo(lift(lhs), liftName(op), lift(rhs))
    case m.Pat.Type.Function(params, res) =>
      untpd("Type.Function").appliedTo(liftSeq(params), lift(res))
    case m.Pat.Type.Tuple(args) =>
      untpd("Type.Tuple").appliedTo(liftSeq(args))
    // case m.PaType.With(lhs, rhs) =>
    case m.Pat.Type.And(lhs, rhs) =>
      untpd("Type.And").appliedTo(lift(lhs), lift(rhs))
    case m.Pat.Type.Or(lhs, rhs) =>
      untpd("Type.Or").appliedTo(lift(lhs), lift(rhs))
    case m.Pat.Type.Refine(tpe, stats) =>
      untpd("Type.Refine").appliedTo(liftOpt(tpe), liftSeq(stats))
    // case m.PaType.Existential(tpe, stats) =>
    case m.Pat.Type.Annotate(tpe, annots) =>
      untpd("Type.Annotated").appliedTo(lift(tpe), liftSeq(annots))
    case m.Pat.Type.Placeholder(bounds) =>
      untpd("Defn.TypeParam").appliedTo(Lit("_"), lift(bounds))

    case m.Decl.Val(mods, pats, tpe) =>
      require(pats.size > 0)
      val modifiers = liftMods(mods)
      liftValDef(modifiers, pats, lift(tpe), null, isDecl = true)
    case m.Decl.Var(mods, pats, tpe) =>
      require(pats.size > 0)
      val modifiers = Select(liftMods(mods), "setMutable")
      liftValDef(modifiers, pats, lift(tpe), null, isDecl = true)
    case m.Decl.Def(mods, name, tparams, paramss, tpe) =>
      untpd("Defn.DefDecl").appliedTo(liftMods(mods), liftName(name), liftSeq(tparams), liftSeqSeq(paramss), lift(tpe))
    case m.Decl.Type(mods, name, tparams, bounds) =>
      untpd("Defn.TypeDecl").appliedTo(liftMods(mods), liftName(name), liftSeq(tparams), scalaSome.appliedTo(lift(bounds)))

    case m.Defn.Val(mods, pats, tpe, rhs) =>
      require(pats.size > 0)
      val modifiers = liftMods(mods)
      liftValDef(modifiers, pats, liftOpt(tpe), lift(rhs), isDecl = false)
    case m.Defn.Var(mods, pats, tpe, rhsOpt) =>
      val rhs: m.Tree = rhsOpt match {
        case Some(t) => t
        case _       => abort("only var definitions with right-hand side supported", enclosingTree.pos)
      }

      require(pats.size > 0)
      val modifiers = Select(liftMods(mods), "setMutable")
      liftValDef(modifiers, pats, liftOpt(tpe), lift(rhs), isDecl = false)
    case m.Defn.Def(mods, name, tparams, paramss, tpe, body) =>
      untpd("Defn.DefDef").appliedTo(liftMods(mods), liftName(name), liftSeq(tparams), liftSeqSeq(paramss), liftOpt(tpe), lift(body))
    // case m.Defn.Macro(mods, name, tparams, paramss, tpe, body) =>
    case m.Defn.Type(mods, name, tparams, body) =>
      untpd("Defn.TypeAlias").appliedTo(liftMods(mods), liftName(name), liftSeq(tparams), lift(body))
    case m.Defn.Class(mods, name, tparams, ctor, m.Template(_, parents, self, stats)) =>
      val (cmods, paramss) = ctor match {
        case m.Ctor.Primary(mods, name, paramss) =>
          (mods, paramss)
      }
      untpd("Defn.Class").appliedTo(
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
      untpd("Defn.Trait").appliedTo(
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
      untpd("Defn.Object").appliedTo(
        liftMods(mods),
        liftName(name),
        liftSeqTrees(parents.map(liftInitCall)),
        liftSelf(self),
        liftOptSeq(stats)
      )

    // case m.Pkg(ref, stats) =>
    // case m.Pkg.Object(mods, name, templ) =>

    case m.Ctor.Secondary(mods, name, paramss, body) =>
      abort("second constructor not supported for patterns", enclosingTree.pos)
    // case m.Ctor.Ref.Name(v) =>                       // handled by liftInitCall
    // case m.Ctor.Ref.Select(qual, name) =>
    // case m.Ctor.Ref.Project(qual, name) =>
    // case m.Ctor.Ref.Function(name) =>                // TODO: what's this?

    // case m.Template(early, parents, self, stats) =>


    case m.Enumerator.Generator(pat, rhs) =>
      untpd("Term.For.GenFrom").appliedTo(lift(pat), lift(rhs))
    case m.Enumerator.Val(pat, rhs) =>
      untpd("Term.For.GenAlias").appliedTo(lift(pat), lift(rhs))
    case m.Enumerator.Guard(cond) =>
      untpd("Term.For.Guard").appliedTo(lift(cond))

    case m.Import(importers) =>
      untpd("Import").appliedTo(liftSeq(importers))
    case m.Importer(ref, importees) =>
      untpd("Import.Item").appliedTo(lift(ref), liftSeq(importees))
    case m.Importee.Wildcard() =>
      untpd("Import.Name").appliedTo(Lit("_"))
    case m.Importee.Name(m.Name.Indeterminate(name)) =>
      untpd("Import.Name").appliedTo(Lit(name))
    case m.Importee.Rename(m.Name.Indeterminate(name), m.Name.Indeterminate(rename)) =>
      untpd("Import.Rename").appliedTo(Lit(name), Lit(rename))
    case m.Importee.Unimport(m.Name.Indeterminate(name)) =>
      untpd("Import.Hide").appliedTo(Lit(name))

    case m.Case(pat, cond, body) =>
      untpd("Term.Case").appliedTo(lift(pat), liftOpt(cond), lift(body))

    // case m.Source(stats) =>
    //  selectFullPath("scala.meta.Source").appliedTo(liftSeq(stats))
  }
}
