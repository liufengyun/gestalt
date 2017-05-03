package scala.gestalt.dotty

import scala.gestalt.{Toolbox => Tbox, Location}

import dotty.tools.dotc._
import core._
import ast.{ untpd => d, Trees => c, tpd }
import StdNames._
import NameOps._
import Contexts._
import Decorators._
import Constants._
import d.modsDeco
import util.Positions
import Positions.Position

import scala.collection.mutable.ListBuffer


class Toolbox(enclosingPosition: Position)(implicit ctx: Context) extends Tbox {

  type Tree = d.Tree
  type TypeTree = d.Tree
  type TermTree = d.Tree
  type DefTree = d.Tree
  type Splice = d.Tree
  type Mods = DottyModifiers

  type Param = d.ValDef
  type TypeParam = d.TypeDef
  type Class = d.TypeDef
  type Trait = d.TypeDef
  type Object = d.ModuleDef
  type ValDef = d.ValDef
  type ValDecl = d.ValDef
  type DefDef = d.DefDef
  type DefDecl = d.DefDef
  type Self = d.ValDef
  type InitCall = d.Tree

  case class DottyModifiers(dottyMods: d.Modifiers) extends Modifiers {
    def isPrivate: Boolean = dottyMods.is(Flags.Private)
    def isProtected: Boolean = dottyMods.is(Flags.Protected)
    def isOverride: Boolean = dottyMods.is(Flags.Override)
    def isFinal: Boolean = dottyMods.is(Flags.Final)
    def isImplicit: Boolean = dottyMods.is(Flags.Implicit)
    def isLazy: Boolean = dottyMods.is(Flags.Lazy)
    def isSealed: Boolean = dottyMods.is(Flags.Sealed)
    def isAbstract: Boolean = dottyMods.is(Flags.Abstract)
    def isCase: Boolean = dottyMods.is(Flags.Case)
    def isContravariant: Boolean = dottyMods.is(Flags.Contravariant)
    def isCovariant: Boolean = dottyMods.is(Flags.Covariant)
    def isInline: Boolean = dottyMods.is(Flags.Inline)
    def isMutable: Boolean = dottyMods.is(Flags.Mutable)

    def isValParam: Boolean =
      dottyMods.is(Flags.Param) &&
        !dottyMods.is(Flags.Mutable) &&
        !dottyMods.is(Flags.allOf(Flags.Private, Flags.Local))

    def isVarParam: Boolean =
      dottyMods.is(Flags.Param) &&
        dottyMods.is(Flags.Mutable) &&
        !dottyMods.is(Flags.allOf(Flags.Private, Flags.Local))

    // can be empty or `this`
    def setPrivate(within: String): Mods =
      if (within == "this") DottyModifiers(dottyMods | Flags.Private | Flags.Local)
      else DottyModifiers(dottyMods.withPrivateWithin(within.toTypeName) | Flags.Private)

    def setProtected(within: String): Mods =
      if (within == "this") DottyModifiers(dottyMods | Flags.Protected | Flags.Local)
      else DottyModifiers(dottyMods.withPrivateWithin(within.toTypeName) | Flags.Protected)

    def setOverride: Mods = DottyModifiers(dottyMods | Flags.Override)
    def setFinal: Mods = DottyModifiers(dottyMods | Flags.Final)
    def setImplicit: Mods = DottyModifiers(dottyMods | Flags.Implicit)
    def setLazy: Mods = DottyModifiers(dottyMods | Flags.Lazy)
    def setSealed: Mods = DottyModifiers(dottyMods | Flags.Sealed)
    def setAbstract: Mods = DottyModifiers(dottyMods | Flags.Abstract)
    def setCase: Mods = DottyModifiers(dottyMods | Flags.Case)
    def setContravariant: Mods = DottyModifiers(dottyMods | Flags.Contravariant)
    def setCovariant: Mods = DottyModifiers(dottyMods | Flags.Covariant)
    def setInline: Mods = DottyModifiers(dottyMods | Flags.Inline)
    def setMutable: Mods = DottyModifiers(dottyMods | Flags.Mutable)

    def setValParam: Mods = DottyModifiers(dottyMods &~ Flags.Local &~ Flags.Mutable)
    def setVarParam: Mods = DottyModifiers(dottyMods &~ Flags.Local | Flags.Mutable)

    def withAddedAnnotation(annot: d.Tree): Mods = DottyModifiers(dottyMods.withAddedAnnotation(annot))
    def withAnnotations(annots: Seq[d.Tree]): Mods = DottyModifiers(dottyMods.withAnnotations(annots.toList))

    def hasAnnotations: Boolean = dottyMods.hasAnnotations

    def annotations: Seq[Tree] = dottyMods.annotations

    def privateWithin: String =
      if (dottyMods.is(Flags.Local)) "this"
      else dottyMods.privateWithin.toString
  }

  // modifiers
  def emptyMods: Mods = DottyModifiers(d.EmptyModifiers)

  def fresh(prefix: String = "$local"): String = ctx.freshName(prefix)

  def getOrEmpty(treeOpt: Option[Tree]): Tree = treeOpt.getOrElse(d.EmptyTree)

  implicit class TreeHelper(tree: d.Tree) {
    def withPosition[T <: Tree] = tree.withPos(enclosingPosition).asInstanceOf[T]
  }

  // diagnostics - the implementation takes the position from the tree
  def error(message: String, tree: Tree): Unit = {
    ctx.error(message, tree.pos)
  }

  /** stop macro transform - the implementation takes the position from the tree */
  def abort(message: String, tree: Tree): Nothing = {
    ctx.error(message, tree.pos)
    throw new Exception(message)
  }

  implicit  def toDotty(tbMods: Mods): d.Modifiers = tbMods.dottyMods

  implicit def fromDotty(dottyMods: d.Modifiers): Mods = DottyModifiers(dottyMods)

  //----------------------------------------


  object AnonymClass extends AnonymClassImpl {
    def apply(parents: Seq[InitCall], selfOpt: Option[Self], stats: Seq[Tree]): Tree = {
      val init = d.DefDef(nme.CONSTRUCTOR, List(), List(), d.TypeTree(), d.EmptyTree)
      val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
      d.Template(init, parents.toList, self, stats).withPosition
    }
  }

  object TypeDecl extends TypeDeclImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], tboundsOpt: Option[TypeTree]): DefTree = {
      val tbounds = tboundsOpt.getOrElse(d.TypeBoundsTree(d.EmptyTree, d.EmptyTree))
      val body =
        if (tparams.size == 0) tbounds
        else d.LambdaTypeTree(tparams.toList.asInstanceOf[List[d.TypeDef]], tbounds)
      d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
    }
  }

  object TypeAlias extends TypeAliasImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], rhs: TypeTree): DefTree = {
      val body =
        if (tparams.size == 0) rhs
        else d.LambdaTypeTree(tparams.toList.asInstanceOf[List[d.TypeDef]], rhs)
      d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
    }
  }

  object PatDef extends PatDefImpl {
    def apply(mods: Mods, lhs: Tree, tpe: Option[TypeTree], rhs: TermTree): DefTree =
      d.PatDef(mods, List(lhs), tpe.getOrElse(d.TypeTree()), rhs).withPosition
  }

  object SeqDef extends SeqDefImpl {
    def apply(mods: Mods, pats: Seq[Tree], tpe: Option[TypeTree], rhs: TermTree): DefTree =
      d.PatDef(mods, pats.toList, tpe.getOrElse(d.TypeTree()), rhs).withPosition
  }

  object SeqDecl extends SeqDeclImpl {
    def apply(mods: Mods, vals: Seq[String], tpe: TypeTree): DefTree =
      d.PatDef(mods, vals.map(n => d.Ident(n.toTermName)).toList, tpe, d.EmptyTree).withPosition
  }

  object SecondaryCtor extends SecondaryCtorImpl {
    def apply(mods: Mods, paramss: Seq[Seq[Param]], rhs: TermTree): DefTree =
      DefDef(mods, nme.CONSTRUCTOR.toString, Nil, paramss, Some(d.TypeTree()), rhs).withPosition
  }

  // qual.T[A, B](x, y)(z)
  object InitCall extends InitCallImpl {
    def apply(qual: Option[Tree], name: String, targs: Seq[TypeTree], argss: Seq[Seq[TermTree]]): InitCall = {
      val select = if (qual.isEmpty) d.Ident(name.toTermName) else d.Select(qual.get, name.toTypeName)
      val fun = if (targs.size == 0) select else TypeApply(select, targs.toList)
      ApplySeq(fun, argss).withPosition
    }
  }

  object Self extends SelfImpl {
    def apply(name: String, tpe: TypeTree): Self =
      d.ValDef(name.toTermName, tpe, d.EmptyTree).withPosition

    def apply(name: String): Self =
      d.ValDef(name.toTermName, d.TypeTree(), d.EmptyTree).withPosition
  }

  // types
  object TypeIdent extends TypeIdentImpl {
    def apply(name: String): TypeTree = d.Ident(name.toTypeName).withPosition
  }

  object TypeSelect extends TypeSelectImpl {
    def apply(qual: Tree, name: String): TypeTree = d.Select(qual, name.toTypeName).withPosition
  }

  object TypeSingleton extends TypeSingletonImpl {
    def apply(ref: Tree): TypeTree = d.SingletonTypeTree(ref).withPosition
  }

  object TypeApply extends TypeApplyImpl {
    def apply(tpe: TypeTree, args: Seq[TypeTree]): TypeTree = d.AppliedTypeTree(tpe, args.toList).withPosition
  }

  object TypeApplyInfix extends TypeApplyInfixImpl {
    def apply(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree =
      d.InfixOp(lhs, d.Ident(op.toTypeName), rhs).withPosition
  }

  object TypeFunction extends TypeFunctionImpl {
    def apply(params: Seq[TypeTree], res: TypeTree): TypeTree =
      d.Function(params.toList, res).withPosition
  }

  object TypeTuple extends TypeTupleImpl {
    def apply(args: Seq[TypeTree]): TypeTree = d.Tuple(args.toList).withPosition
  }

  object TypeAnd extends TypeAndImpl {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree =
      d.AndTypeTree(lhs, rhs).withPosition
  }

  object TypeOr extends TypeOrImpl {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree =
      d.OrTypeTree(lhs, rhs).withPosition
  }

  object TypeRefine extends TypeRefineImpl {
    def apply(tpe: Option[TypeTree], stats: Seq[Tree]): TypeTree =
      d.RefinedTypeTree(tpe.getOrElse(d.EmptyTree), stats.toList).withPosition
  }

  object TypeBounds extends TypeBoundsImpl {
    def apply(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree = {
      require(lo.nonEmpty || hi.nonEmpty)
      d.TypeBoundsTree(lo.getOrElse(d.EmptyTree), hi.getOrElse(d.EmptyTree)).withPosition
    }
  }

  object TypeRepeated extends TypeRepeatedImpl {
    def apply(tpe: TypeTree): TypeTree =
      d.PostfixOp(tpe, d.Ident(nme.raw.STAR)).withPosition
  }

  object TypeByName extends TypeByNameImpl {
    def apply(tpe: TypeTree): TypeTree =
      d.ByNameTypeTree(tpe).withPosition
  }

  object TypeAnnotated extends TypeAnnotatedImpl {
    def apply(tpe: TypeTree, annots: Seq[Tree]): TypeTree = {
      require(annots.size > 0)
      annots.tail.foldRight(d.Annotated(tpe, annots.head).withPosition) { (ann: Tree, acc: TypeTree) =>
        d.Annotated(acc, ann).withPosition[TypeTree]
      }
    }
  }

  // terms

  object Super extends SuperImpl {
    def apply(thisp: String, superp: String): TermTree =
      d.Super(d.Ident(thisp.toTypeName), d.Ident(superp.toTypeName)).withPosition
  }

  object Interpolate extends InterpolateImpl {
    def apply(prefix: String, parts: Seq[String], args: Seq[TermTree]): TermTree = {
      val thickets =
        for {(arg, part) <- args.zip(parts.take(args.size))}
          yield {
            val expr = arg match {
              case tree: d.Ident => tree
              case tree => d.Block(Nil, tree)
            }
            d.Thicket(Lit(part), expr)
          }
      val segments =
        if (parts.size > args.size)
          thickets :+ Lit(parts.last)
        else thickets

      d.InterpolatedString(prefix.toTermName, segments.toList).withPosition
    }
  }


  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  object Infix extends InfixImpl {
    def apply(lhs: TermTree, op: String, rhs: TermTree): TermTree =
      d.Apply(d.Select(lhs, op.toTermName), List(rhs)).withPosition
  }

  object Prefix extends PrefixImpl {
    def apply(op: String, od: TermTree): TermTree = d.PrefixOp(d.Ident(op.toTermName), od).withPosition
  }

  object Postfix extends PostfixImpl {
    def apply(od: TermTree, op: String): TermTree =
      d.PostfixOp(od, d.Ident(op.toTermName)).withPosition
  }

  object Return extends ReturnImpl {
    def apply(expr: TermTree): TermTree = d.Return(expr, d.EmptyTree).withPosition
    def apply: TermTree = d.Return(d.EmptyTree, d.EmptyTree).withPosition
    def unapply(tree: Tree): Option[Option[TermTree]] = tree match {
      case c.Return(expr, _) => Some(if (expr.isEmpty) None else Some(expr))
      case _ => None
    }
  }

  object Throw extends ThrowImpl {
    def apply(expr: TermTree): TermTree = d.Throw(expr).withPosition
  }

  object If extends IfImpl {
    def apply(cond: TermTree, thenp: TermTree, elsep: Option[TermTree]): TermTree =
      d.If(cond, thenp, elsep.getOrElse(d.EmptyTree)).withPosition

    def unapply(tree: Tree): Option[(TermTree, TermTree, Option[TermTree])] = tree match {
      case c.If(cond, thenp, elsep) =>
        Some(cond, thenp, if (elsep.isEmpty) None else Some(elsep))
      case _ => None
    }
  }

  object Try extends TryImpl {
    def apply(expr: TermTree, cases: Seq[Tree], finallyp: Option[TermTree]): TermTree =
      d.Try(expr, cases.toList.asInstanceOf[List[d.CaseDef]], finallyp.getOrElse(d.EmptyTree)).withPosition

    def apply(expr: TermTree, handler: Tree, finallyp: Option[TermTree]): TermTree =
      d.ParsedTry(expr, handler, finallyp.getOrElse(d.EmptyTree)).withPosition
  }

  object Function extends FunctionImpl {
    def apply(params: Seq[Param], body: TermTree): TermTree =
      d.Function(params.toList, body).withPosition
  }

  object While extends WhileImpl {
    def apply(expr: TermTree, body: TermTree): TermTree = d.WhileDo(expr, body).withPosition
  }

  object DoWhile extends DoWhileImpl {
    def apply(body: TermTree, expr: TermTree): TermTree = d.DoWhile(body, expr).withPosition
  }

  object For extends ForImpl {
    def ForDo(enums: Seq[Tree], body: TermTree): TermTree = d.ForDo(enums.toList, body)
    def ForYield(enums: Seq[Tree], body: TermTree): TermTree = d.ForYield(enums.toList, body)
    def GenFrom(pat: TermTree, rhs: TermTree): Tree = d.GenFrom(pat, rhs)
    def GenAlias(pat: TermTree, rhs: TermTree): Tree = d.GenAlias(pat, rhs)
    def Guard(cond: TermTree): Tree = cond
  }

  object New extends NewImpl {
    // can be InitCall or AnonymClass
    def apply(tpe: Tree): TermTree = d.New(tpe).withPosition
  }

  object Named extends NamedImpl {
    def apply(name: String, expr: TermTree): TermTree =
      d.NamedArg(name.toTermName, expr).withPosition
  }

  object Repeated extends RepeatedImpl {
    def apply(expr: TermTree): TermTree =
      d.Typed(expr, d.Ident(tpnme.WILDCARD_STAR)).withPosition
  }

  // patterns
  object Bind extends BindImpl {
    def apply(name: String, expr: TermTree): TermTree =
      d.Bind(name.toTermName, expr).withPosition
  }

  object Alternative extends AlternativeImpl {
    def apply(trees: Seq[TermTree]): TermTree =
      d.Alternative(trees.toList).withPosition
  }

  // importees
  object Import extends ImportImpl {
    def apply(items: Seq[Tree]): Tree =
      if (items.size == 1)
        items.head.withPosition
      else
        d.Thicket(items.toList).withPosition

    def Item(ref: Tree, importees: Seq[Tree]): Tree =
      d.Import(ref, importees.toList).withPosition

    def Name(name: String): Tree = d.Ident(name.toTermName).withPosition

    def Rename(from: String, to: String): Tree =
      d.Thicket(d.Ident(from.toTermName), d.Ident(to.toTermName)).withPosition

    def Hide(name: String): Tree =
      d.Thicket(d.Ident(name.toTermName), d.Ident(nme.WILDCARD)).withPosition
  }

  // extractors
  object Lit extends LitImpl {
    def apply(value: Any): TermTree = d.Literal(Constant(value)).withPosition
    def unapply(tree: Tree): Option[Any] = tree match {
      case c.Literal(Constant(v)) => Some(v)
      case _ => None
    }
  }

  object Ident extends IdentImpl {
    def apply(name: String): TermTree = d.Ident(name.toTermName).withPosition
    def unapply(tree: Tree): Option[String] = tree match {
      case c.Ident(name) if name.isTermName => Some(name.show)
      case _ => None
    }
  }

  object This extends ThisImpl {
    def apply(qual: String): TermTree = d.This(d.Ident(qual.toTypeName)).withPosition
    def unapply(tree: Tree): Option[String] = tree match {
      case c.This(c.Ident(name)) => Some(name.show)
      case _ => None
    }
  }

  object Select extends SelectImpl {
    def apply(qual: TermTree, name: String): TermTree = d.Select(qual, name.toTermName).withPosition
    def unapply(tree: Tree): Option[(TermTree, String)] = tree match {
      case c.Select(qual, name) if name.isTermName => Some((qual, name.show))
      case _ => None
    }
  }

  object Apply extends ApplyImpl {
    def apply(fun: TermTree, args: Seq[TermTree]): TermTree = d.Apply(fun, args.toList).withPosition

    def unapply(tree: Tree): Option[(TermTree, Seq[TermTree])] = tree match {
      case c.Apply(fun, Seq(c.Typed(c.SeqLiteral(args, _), _))) => Some((fun, args))
      case c.Apply(fun, args) => Some((fun, args))
      case _ => None
    }
  }

  object Ascribe extends AscribeImpl {
    def apply(expr: TermTree, tpe: TypeTree): TermTree = d.Typed(expr, tpe).withPosition

    def unapply(tree: Tree): Option[(TermTree, TypeTree)] = tree match {
      case c.Typed(expr, tpe) => Some((expr, tpe))
      case _ => None
    }
  }

  object Assign extends AssignImpl {
    def apply(lhs: TermTree, rhs: TermTree): TermTree = d.Assign(lhs, rhs).withPosition

    def unapply(tree: Tree): Option[(TermTree, TermTree)] = tree match {
      case c.Assign(lhs, rhs) => Some((lhs, rhs))
      case _ => None
    }
  }

  object Annotated extends AnnotatedImpl {
    def apply(expr: TermTree, annots: Seq[Tree]): TermTree = {
      require(annots.size > 0)
      annots.tail.foldRight(d.Annotated(expr, annots.head).withPosition) { (ann: Tree, acc: TermTree) =>
        d.Annotated(acc, ann).withPosition[TermTree]
      }
    }

    def unapply(tree: Tree): Option[(TermTree, Seq[Tree])] = {
      def recur(term: Tree, acc: Seq[Tree]): (Tree, Seq[Tree])  = term match {
        case c.Annotated(expr, annot) => recur(expr, annot +: acc) // inner-most is in the front
        case expr => (expr, acc)
      }
      val (expr, annots) = recur(tree, Nil)
      if (annots.size > 0) Some((expr, annots))
      else None
    }
  }

  object Block extends BlockImpl {
    def apply(stats: Seq[Tree]): TermTree = {
      if (stats.size == 0)
        d.Block(stats.toList, d.EmptyTree).withPosition
      else
        d.Block(stats.init.toList, stats.last).withPosition
    }

    def unapply(tree: Tree): Option[Seq[Tree]] = tree match {
      case c.Block(stats, expr) => Some(stats :+ expr)
      case _ => None
    }
  }

  object Match extends MatchImpl {
    def apply(expr: TermTree, cases: Seq[Tree]): TermTree =
      d.Match(expr, cases.toList.asInstanceOf[List[d.CaseDef]]).withPosition

    def unapply(tree: Tree): Option[(TermTree, Seq[Tree])] = tree match {
      case c.Match(expr, cases) => Some((expr, cases))
      case _ => None
    }
  }

  object Case extends CaseImpl {
    def apply(pat: TermTree, cond: Option[TermTree], body: TermTree): Tree =
      d.CaseDef(pat, cond.getOrElse(d.EmptyTree), body).withPosition

    def unapply(tree: Tree): Option[(TermTree, Option[TermTree], TermTree)] = tree match {
      case c.CaseDef(pat, cond, body) =>
        val condOpt = if (cond == d.EmptyTree) None else Some(cond)
        Some((pat, condOpt, body))
      case _ => None
    }
  }

  object PartialFunction extends PartialFunctionImpl {
    def apply(cases: Seq[Tree]): TermTree =
      d.Match(d.Thicket(Nil), cases.toList.asInstanceOf[List[d.CaseDef]]).withPosition

    def unapply(tree: Tree): Option[Seq[Tree]] = tree match {
      case c.Match(c.Thicket(Nil), cases) => Some(cases)
      case _ => None
    }
  }

  object Tuple extends TupleImpl {
    def apply(args: Seq[TermTree]): TermTree = d.Tuple(args.toList).withPosition
    def unapply(tree: Tree): Option[Seq[TermTree]] = tree match {
      case d.Tuple(trees) => Some(trees)
      case _ => None
    }
  }


  object SeqLiteral extends SeqLiteralImpl {
    def unapply(tree: Tree): Option[Seq[TermTree]] = tree match {
      case c.Typed(c.SeqLiteral(elems,_), _) => Some(elems)
      case _ => None
    }
  }

  object ApplyType extends ApplyTypeImpl {
    def apply(fun: Tree, args: Seq[TypeTree]): TermTree = d.TypeApply(fun, args.toList).withPosition
    def unapply(tree: Tree): Option[(TermTree, Seq[TypeTree])] = tree match {
      case c.TypeApply(fun, args) => Some((fun, args))
      case _ => None
    }
  }


  object Object extends ObjectImpl {
    def apply(mods: Mods, name: String, parents: Seq[InitCall], selfOpt: Option[Self], stats: Seq[Tree]): Object = {
      val constr = d.DefDef(nme.CONSTRUCTOR, Nil, Nil, d.TypeTree(), d.EmptyTree)
      val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
      val templ = d.Template(constr, parents.toList, self, stats)
      d.ModuleDef(name.toTermName, templ).withMods(mods).withPosition
    }

    def unapply(tree: Tree): Option[(Mods, String, Seq[InitCall], Option[Self], Seq[Tree])] = tree match {
      case obj @ d.ModuleDef(name, templ @ c.Template(constr, parents, self: d.ValDef, body)) =>
        val selfOpt = if (self == d.EmptyValDef) None else Some(Self(self.name.toString, self.tpt))
        Some((mods(obj), name.toString, parents, selfOpt, templ.body))
      case _ => None
    }

    def mods(tree: Object): Mods = new modsDeco(tree).mods
    def name(tree: Object): String = tree.name.toString
    def parents(tree: Object): Seq[InitCall] = tree match {
      case d.ModuleDef(_, c.Template(_, parents, _, _)) => parents
    }

    def selfOpt(tree: Object): Option[Self] = tree match {
      case d.ModuleDef(_, c.Template(_, _, self, _)) => if (self.isEmpty) None else Some(self)
    }

    def stats(tree: Object): Seq[Tree] = tree match {
      case d.ModuleDef(_, tmpl @ c.Template(_, _, _, _)) => tmpl.forceIfLazy
    }

    def copyStats(tree: Object)(stats: Seq[Tree]) = {
      val tmpl = d.cpy.Template(tree.impl)(body = stats)
      d.cpy.ModuleDef(tree)(name = tree.name, impl = tmpl)
    }

    def get(tree: Tree): Option[Object] = tree match {
      case obj @ d.ModuleDef(_, c.Template(self, _, _, _)) => Some(obj)
      case _ => None
    }
  }

  object Class extends ClassImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], ctorMods: Mods, paramss: Seq[Seq[Param]], parents: Seq[InitCall], selfOpt: Option[Self], stats: Seq[Tree]): Class = {
      val constr =
        if (paramss.isEmpty) d.DefDef(nme.CONSTRUCTOR, Nil, Nil, d.TypeTree(), d.EmptyTree)
        else {
          val tparamsCast = tparams.toList.asInstanceOf[List[d.TypeDef]]
          val paramssCast = paramss.map(_.toList).toList.asInstanceOf[List[List[d.ValDef]]]
          d.DefDef(nme.CONSTRUCTOR, tparamsCast, paramssCast, d.TypeTree(), d.EmptyTree).withMods(ctorMods)
        }

      val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
      val templ = d.Template(constr, parents.toList, self, stats)
      d.TypeDef(name.toTypeName, templ).withMods(mods).withPosition
    }

    def unapply(tree: Tree): Option[(Mods, String, Seq[TypeParam], Mods, Seq[Seq[Param]], Seq[InitCall], Option[Self], Seq[Tree])] = tree match {
      case cdef @ c.TypeDef(name, templ @ c.Template(constr, parents, self, body)) =>
        var tparams: List[TypeParam] = Nil
        val (paramss, cmods) = constr match {
          case c.DefDef(nme.CONSTRUCTOR, Nil, Nil, c.TypeTree(), d.EmptyTree) => (Nil, emptyMods)
          case pctor : d.DefDef =>
            tparams = pctor.tparams
            (pctor.vparamss, (new modsDeco(pctor).mods): Mods)
        }
        val selfOpt = if (self == d.EmptyValDef) None else Some(self)
        Some((mods(cdef), name.toString, tparams, cmods, paramss, parents, selfOpt, templ.body))
      case _ => None
    }

    def mods(tree: Class): Mods = new modsDeco(tree).mods
    def name(tree: Class): String = tree.name.toString
    def parents(tree: Class): Seq[InitCall] = tree match {
      case c.TypeDef(_, c.Template(_, parents, _, _)) => parents
    }

    def ctorMods(tree: Class): Mods = tree match {
      case c.TypeDef(_, c.Template(ctor, _, _, _)) => new modsDeco(ctor).mods
    }

    def paramss(tree: Class): Seq[Seq[Param]] = tree match {
      case c.TypeDef(_, c.Template(ctor, _, _, _)) => ctor.vparamss
    }

    def tparams(tree: Class): Seq[TypeParam] = tree match {
      case c.TypeDef(_, c.Template(ctor, _, _, _)) => ctor.tparams
    }

    def selfOpt(tree: Class): Option[Self] = tree match {
      case c.TypeDef(_, c.Template(_, _, self, _)) => if (self.isEmpty) None else Some(self)
    }

    def stats(tree: Class): Seq[Tree] = tree match {
      case c.TypeDef(_, tmpl : d.Template) => tmpl.forceIfLazy
    }

    def copyMods(tree: Class)(mods: Mods) = tree.withMods(mods)

    def copyParamss(tree: Class)(paramss: Seq[Seq[Param]]) = {
      val tmpl1 = tree.rhs.asInstanceOf[d.Template]
      val contr2 = d.cpy.DefDef(tmpl1.constr)(vparamss = paramss.toList.map(_.toList))
      val tmpl2 = d.cpy.Template(tmpl1)(constr = contr2)
      d.cpy.TypeDef(tree)(rhs = tmpl1)
    }

    def copyStats(tree: Class)(stats: Seq[Tree]) = {
      val tmpl1 = tree.rhs.asInstanceOf[d.Template]
      val tmpl2 = d.cpy.Template(tmpl1)(body = stats)
      d.cpy.TypeDef(tree)(rhs = tmpl2)
    }

    def get(tree: Tree): Option[Class] = tree match {
      case cdef @ c.TypeDef(_, _ : d.Template) if !mods(cdef).is(Flags.Trait) => Some(cdef)
      case _ => None
    }
  }

  object Trait extends TraitImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], ctorMods: Mods, paramss: Seq[Seq[Param]], parents: Seq[InitCall], self: Option[Self], stats: Seq[Tree]): Trait =
      Class(mods.dottyMods | Flags.Trait, name, tparams, ctorMods, paramss, parents, self, stats).withPosition

    def unapply(tree: Tree): Option[(Mods, String, Seq[TypeParam], Mods, Seq[Seq[Param]], Seq[InitCall], Option[Self], Seq[Tree])] =
      if (!tree.isInstanceOf[d.TypeDef]) return None
      else {
        val typedef = tree.asInstanceOf[d.TypeDef]
        if (!typedef.mods.is(Flags.Trait)) return None

        Class.unapply(typedef)
      }

    def mods(tree: Trait): Mods = new modsDeco(tree).mods
    def name(tree: Trait): String = tree.name.toString
    def parents(tree: Trait): Seq[InitCall] = Trait.parents(tree)

    def paramss(tree: Trait): Seq[Seq[Param]] = Class.paramss(tree)

    def tparams(tree: Trait): Seq[TypeParam] = Class.tparams(tree)

    def selfOpt(tree: Trait): Option[Self] = Class.selfOpt(tree)

    def stats(tree: Trait): Seq[Tree] = Class.stats(tree)

    def copyStats(tree: Trait)(stats: Seq[Tree]) = Class.copyStats(tree)(stats)

    def get(tree: Tree): Option[Trait] = tree match {
      case cdef @ c.TypeDef(_, _ : d.Template) if mods(cdef).is(Flags.Trait)  => Some(cdef)
      case _ => None
    }
  }

   // accessors
  object Param extends ParamImpl {
    def apply(mods: Mods, name: String, tpe: Option[TypeTree], default: Option[TermTree]): Param = {
      d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), getOrEmpty(default))
        .withMods(mods.dottyMods | Flags.TermParam).withPosition.asInstanceOf[Param]
    }

    def mods(tree: Param): Mods = new modsDeco(tree).mods
    def name(tree: Param): String = tree.name.show
    def tptOpt(tree: Param): Option[TypeTree] = if (tree.tpt.isInstanceOf[d.TypeTree]) None else Some(tree.tpt)
    def defaultOpt(tree: Param): Option[TermTree] = if (tree.forceIfLazy == d.EmptyTree) None else Some(tree.forceIfLazy)
    def copyMods(tree: Param)(mods: Mods): Param = tree.withMods(mods)
  }

  object TypeParam extends TypeParamImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], tboundsOpt: Option[TypeTree], cbounds: Seq[TypeTree]): TypeParam = {
      val tbounds = tboundsOpt.getOrElse(d.TypeBoundsTree(d.EmptyTree, d.EmptyTree)).asInstanceOf[d.TypeBoundsTree]
      val inner =
        if (cbounds.size == 0) tbounds
        else d.ContextBounds(tbounds, cbounds.toList.map(d.AppliedTypeTree(_, d.Ident(name.toTypeName))))

      val body =
        if (tparams.size == 0) inner
        else d.LambdaTypeTree(tparams.toList.asInstanceOf[List[d.TypeDef]], inner)

      d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
    }

    def mods(tree: TypeParam): Mods = new modsDeco(tree).mods
    def name(tree: TypeParam): String = tree.name.show
    def tparams(tree: TypeParam): Seq[TypeParam] = tree match {
      case c.TypeDef(_, c.LambdaTypeTree(tparams, _)) => tparams
      case c.TypeDef(_, _) => Nil // bounds
    }
  }

  object ValDef extends ValDefImpl {
    def apply(mods: Mods, name: String, tpe: Option[TypeTree], rhs: TermTree): ValDef =
      d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), rhs).withMods(mods).withPosition

    def mods(tree: ValDef): Mods = new modsDeco(tree).mods
    def name(tree: ValDef): String = tree.name.show
    def rhs(tree: ValDef): TermTree = tree.forceIfLazy
    def tptOpt(tree: ValDef): Option[TypeTree] = if (tree.tpt.isInstanceOf[d.TypeTree]) None else Some(tree.tpt)
    def copyRhs(tree: ValDef)(rhs: TermTree): ValDef = d.cpy.ValDef(tree)(rhs = rhs)

    def get(tree: Tree): Option[ValDef] = tree match {
      case vdef : d.ValDef if !vdef.forceIfLazy.isEmpty => Some(vdef)
      case _ => None
    }

    def unapply(tree: Tree): Option[(Mods, String, Option[TypeTree], TermTree)] = tree match {
      case vdef : d.ValDef if !vdef.forceIfLazy.isEmpty =>
        Some((mods(vdef), name(vdef), tptOpt(vdef), rhs(vdef)))
      case _ => None
    }
  }

  object ValDecl extends ValDeclImpl {
    def apply(mods: Mods, name: String, tpe: TypeTree): ValDecl =
      d.ValDef(name.toTermName, tpe, d.EmptyTree).withMods(mods).withPosition

    def mods(tree: ValDecl): Mods = new modsDeco(tree).mods
    def name(tree: ValDecl): String = tree.name.show
    def tpt(tree: ValDecl): TypeTree = tree.tpt

    def get(tree: Tree): Option[ValDecl] = tree match {
      case vdef : d.ValDef if vdef.forceIfLazy.isEmpty  => Some(vdef)
      case _ => None
    }

    def unapply(tree: Tree): Option[(Mods, String, TypeTree)] = tree match {
      case vdef : d.ValDef if vdef.forceIfLazy.isEmpty  =>
        Some((mods(vdef), name(vdef), vdef.tpt))
      case _ => None
    }
  }

  object DefDef extends DefDefImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], paramss: Seq[Seq[Param]], tpe: Option[TypeTree], rhs: TermTree): DefDef = {
      val types = tparams.toList
      val params = paramss.map(_.toList).toList.asInstanceOf[List[List[d.ValDef]]]
      d.DefDef(name.toTermName, types, params, tpe.getOrElse(d.TypeTree()), rhs).withMods(mods).withPosition
    }

    def mods(tree: DefDef): Mods = new modsDeco(tree).mods
    def name(tree: DefDef): String = tree.name.show
    def tptOpt(tree: DefDef): Option[TypeTree] = if (tree.tpt.isInstanceOf[d.TypeTree]) None else Some(tree.tpt)
    def tparams(tree: DefDef): Seq[TypeParam] = tree.tparams
    def paramss(tree: DefDef): Seq[Seq[Param]] = tree.vparamss
    def rhs(tree: DefDef): TermTree = tree.forceIfLazy
    def copyRhs(tree: DefDef)(rhs: TermTree): DefDef = d.cpy.DefDef(tree)(rhs = rhs)

    def get(tree: Tree): Option[DefDef] = tree match {
      case ddef : d.DefDef if !ddef.forceIfLazy.isEmpty  => Some(ddef)
      case _ => None
    }

    def unapply(tree: Tree): Option[(Mods, String, Seq[TypeParam], Seq[Seq[Param]], Option[TypeTree], TermTree)] = tree match {
      case ddef : d.DefDef if !ddef.forceIfLazy.isEmpty  =>
        Some((mods(ddef), name(ddef), ddef.tparams, ddef.vparamss, tptOpt(ddef), rhs(ddef)))
      case _ => None
    }
  }

  object DefDecl extends DefDeclImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], paramss: Seq[Seq[Param]], tpe: TypeTree): DefDecl = {
      val types = tparams.toList
      val params = paramss.map(_.toList).toList
      d.DefDef(name.toTermName, types, params, tpe, d.EmptyTree).withMods(mods).withPosition
    }

    def get(tree: Tree): Option[DefDecl] = tree match {
      case ddef : d.DefDef if ddef.forceIfLazy.isEmpty => Some(ddef)
      case _ => None
    }

    def unapply(tree: Tree): Option[(Mods, String, Seq[TypeParam], Seq[Seq[Param]], TypeTree)] = tree match {
      case ddef : d.DefDef if ddef.forceIfLazy.isEmpty =>
        Some((mods(ddef), name(ddef), ddef.tparams, ddef.vparamss, ddef.tpt))
      case _ => None
    }

    def mods(tree: DefDecl): Mods = new modsDeco(tree).mods
    def name(tree: DefDecl): String = tree.name.show
    def tparams(tree: DefDecl): Seq[TypeParam] = tree.tparams
    def paramss(tree: DefDecl): Seq[Seq[Param]] = tree.vparamss
    def tpt(tree: DefDecl): TypeTree = tree.tpt
  }

  object TypedSplice extends TypedSpliceImpl {
    def apply(tree: Tree): Splice = d.TypedSplice(tree.asInstanceOf[tpd.Tree])
  }


  /*------------------------------- traversers -------------------------------------*/
  def traverse(tree: Tree)(pf: PartialFunction[Tree, Unit]): Unit =
     new d.UntypedTreeMap() {
      override def transform(tree: Tree)(implicit ctx: Context) = {
        pf.lift(tree).getOrElse(super.transform(tree))
        tree
      }
    }.transform(tree)

  def exists(tree: Tree)(pf: PartialFunction[Tree, Boolean]): Boolean = {
    var r = false
    traverse(tree) {
      case t if pf.isDefinedAt(t) && !r => r = pf(t)
    }
    r
  }

  def transform(tree: Tree)(pf: PartialFunction[Tree, Tree]): Tree = {
    new d.UntypedTreeMap() {
      override def transform(tree: Tree)(implicit ctx: Context) =
        pf.lift(tree).getOrElse(super.transform(tree))
    }.transform(tree)
  }

  /*------------------------------- types -------------------------------------*/

  type Type = Types.Type
  type MethodType = Types.MethodType
  type Symbol = Symbols.Symbol

  /** get the location where the def macro is used */
  def currentLocation: Location = Location(ctx.compilationUnit.source.file.name, enclosingPosition.line(), enclosingPosition.column())

  object Type extends TypeImpl {

    /** pretty print type */
    def show(tp: Type): String = tp.show

    /** are the two types equal? */
    def =:=(tp1: Type, tp2: Type): Boolean = tp1 =:= tp2

    /** is `tp1` a subtype of `tp2` */
    def <:<(tp1: Type, tp2: Type): Boolean = tp1 <:< tp2

    /** returning a type referring to a type definition */
    def typeRef(path: String): Type = ctx.staticRef(path.toTypeName, false).symbol.typeRef

    /** returning a type referring to a term definition */
    def termRef(path: String): Type = ctx.staticRef(path.toTermName, false).symbol.termRef

    /** type associated with the tree */
    def typeOf(tree: Tree): Type = tree.tpe

    /** does the type refer to a case class? */
    def isCaseClass(tp: Type): Boolean = tp.classSymbol.is(Flags.Case)

    /** val fields of a case class Type -- only the ones declared in primary constructor */
    def caseFields(tp: Type): Seq[Denotation] = {
      tp.memberDenots(
        Types.fieldFilter,
        (name, buf) => buf ++= tp.member(name).altsWith(_ is Flags.ParamAccessor)
      )
    }

    /* field with the given name */
    def fieldIn(tp: Type, name: String): Option[Denotation] = {
      tp.memberExcluding(name.toTermName, Flags.Method).altsWith(
        p => p.owner == tp.widenSingleton.classSymbol
      ).headOption
    }

    def fieldsIn(tp: Type): Seq[Denotation] = {
      tp.memberDenots(
        Types.fieldFilter,
        (name, buf) => buf ++= tp.member(name).altsWith(
          p => !p.is(Flags.Method) && p.owner == tp.widenSingleton.classSymbol
        )
      )
    }

    def method(tp: Type, name: String): Seq[Denotation] = {
      tp.member(name.toTermName).altsWith(p => p.is(Flags.Method))
    }

    def methods(tp: Type): Seq[Denotation] = {
      tp.memberDenots(
        Types.takeAllFilter,
        (name, buf) => buf ++= tp.member(name).altsWith(p => p.is(Flags.Method) && !p.isConstructor)
      )
    }

    def methodIn(tp: Type, name: String): Seq[Denotation] = {
      tp.member(name.toTermName).altsWith(
        p => p.is(Flags.Method) && p.owner == tp.widenSingleton.classSymbol
      )
    }

    def methodsIn(tp: Type): Seq[Denotation] = {
      tp.memberDenots(
        Types.takeAllFilter,
        (name, buf) => buf ++= tp.member(name).altsWith(
          p => p.is(Flags.Method) && p.owner == tp.widenSingleton.classSymbol && !p.isConstructor
        )
      )
    }

    def companion(tp: Type): Option[Type] = {
      val clazz = tp.widenSingleton.classSymbol
      if (clazz.exists)
        if (clazz.is(Flags.Module) && clazz.companionClass.exists)
          Some(clazz.companionClass.namedType)
        else if (!clazz.is(Flags.Module) && clazz.companionModule.exists)
          Some(clazz.companionModule.namedType)
        else None
      else None
    }

    def widen(tp: Type): Type = tp.widen

    def denot(tp: Type): Option[Denotation] = tp match {
      case tp: Types.NamedType => Some(tp.denot)
      case tp: Types.TypeProxy => denot(tp.underlying)
      case _ => None
    }
  }

  object ByNameType extends ByNameTypeImpl {
    def unapply(tp: Type): Option[Type] = tp match {
      case tp: Types.ExprType => Some(tp.underlying)
      case _ => None
    }
  }

  object MethodType extends MethodTypeImpl {
    def paramInfos(tp: MethodType): Seq[Type] = tp.paramInfos
    def instantiate(tp: MethodType)(params: Seq[Type]): Type = {
      tp.instantiate(params.toList)
    }
    def unapply(tp: Type): Option[MethodType] = tp match {
      case tp: Types.MethodType => Some(tp)
      case _ => None
    }
  }

  /*------------------------------- symbols -------------------------------------*/

  object Symbol extends SymbolImpl {
    /** name of a member */
    def name(mem: Symbol): String = mem.showName

    /** type of a member with respect to a prefix */
    def asSeenFrom(mem: Symbol, prefix: Type): Type = mem.asSeenFrom(prefix)
  }

  /*------------------------------- Denotations -------------------------------------*/
  type Denotation = Denotations.Denotation

  object Denotation extends DenotationImpl {
    def name(denot: Denotation): String = denot.symbol.name.show

    def info(denot: Denotation): Type = denot.info.dealias

    def symbol(denot: Denotation): Symbol = denot.symbol
  }
}
