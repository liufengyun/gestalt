package scala.gestalt.dotty

import scala.gestalt.{Toolbox => Tbox, StructToolbox => STbox, TypeToolbox => TTbox, Location}

import dotty.tools.dotc._
import core._
import ast.{ untpd => d, Trees => c }
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
        !dottyMods.is(Flags.allOf(Flags.Private | Flags.Local))

    def isVarParam: Boolean =
      dottyMods.is(Flags.Param) &&
        dottyMods.is(Flags.Mutable) &&
        !dottyMods.is(Flags.allOf(Flags.Private | Flags.Local))

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

  implicit  def toDotty(tbMods: Mods): d.Modifiers = tbMods.dottyMods

  implicit def fromDotty(dottyMods: d.Modifiers): Mods = DottyModifiers(dottyMods)

  //----------------------------------------

  def Object(mods: Mods, name: String, parents: Seq[InitCall], selfOpt: Option[Self], stats: Seq[Tree]): Object = {
    val constr = d.DefDef(nme.CONSTRUCTOR, Nil, Nil, d.TypeTree(), d.EmptyTree)
    val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
    val templ = d.Template(constr, parents.toList, self, stats)
    d.ModuleDef(name.toTermName, templ).withMods(mods).withPosition
  }

  def Class(mods: Mods, name: String, tparams: Seq[TypeParam], ctorMods: Mods, paramss: Seq[Seq[Param]], parents: Seq[InitCall], selfOpt: Option[Self], stats: Seq[Tree]): Class = {
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

  def AnonymClass(parents: Seq[InitCall], selfOpt: Option[Self], stats: Seq[Tree]): Tree = {
    val init = d.DefDef(nme.CONSTRUCTOR, List(), List(), d.TypeTree(), d.EmptyTree)
    val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
    d.Template(init, parents.toList, self, stats).withPosition
  }

  def Trait(mods: Mods, name: String, tparams: Seq[TypeParam], ctorMods: Mods, paramss: Seq[Seq[Param]], parents: Seq[InitCall], self: Option[Self], stats: Seq[Tree]): Trait =
    Class(mods, name, tparams, ctorMods, paramss, parents, self, stats).asInstanceOf[d.TypeDef].withFlags(Flags.Trait).withPosition

  def TypeDecl(mods: Mods, name: String, tparams: Seq[TypeParam], tboundsOpt: Option[TypeTree]): DefTree = {
    val tbounds = tboundsOpt.getOrElse(d.TypeBoundsTree(d.EmptyTree, d.EmptyTree))
    val body =
      if (tparams.size == 0) tbounds
      else d.LambdaTypeTree(tparams.toList.asInstanceOf[List[d.TypeDef]], tbounds)
    d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
  }

  def TypeAlias(mods: Mods, name: String, tparams: Seq[TypeParam], rhs: TypeTree): DefTree = {
    val body =
      if (tparams.size == 0) rhs
      else d.LambdaTypeTree(tparams.toList.asInstanceOf[List[d.TypeDef]], rhs)
    d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
  }

  def DefDef(mods: Mods, name: String, tparams: Seq[TypeParam], paramss: Seq[Seq[Param]], tpe: Option[TypeTree], rhs: TermTree): DefDef = {
    val types = tparams.toList
    val params = paramss.map(_.toList).toList.asInstanceOf[List[List[d.ValDef]]]
    d.DefDef(name.toTermName, types, params, tpe.getOrElse(d.TypeTree()), rhs).withMods(mods).withPosition
  }

  def DefDecl(mods: Mods, name: String, tparams: Seq[TypeParam], paramss: Seq[Seq[Param]], tpe: TypeTree): DefDecl = {
    val types = tparams.toList
    val params = paramss.map(_.toList).toList
    d.DefDef(name.toTermName, types, params, tpe, d.EmptyTree).withMods(mods).withPosition
  }

  def ValDef(mods: Mods, name: String, tpe: Option[TypeTree], rhs: TermTree): ValDef =
    d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), rhs).withMods(mods).withPosition

  def PatDef(mods: Mods, lhs: Tree, tpe: Option[TypeTree], rhs: TermTree): DefTree =
    d.PatDef(mods, List(lhs), tpe.getOrElse(d.TypeTree()), rhs).withPosition

  def SeqDef(mods: Mods, pats: Seq[Tree], tpe: Option[TypeTree], rhs: TermTree): DefTree =
    d.PatDef(mods, pats.toList, tpe.getOrElse(d.TypeTree()), rhs).withPosition

  def ValDecl(mods: Mods, name: String, tpe: TypeTree): ValDecl =
    d.ValDef(name.toTermName, tpe, d.EmptyTree).withMods(mods).withPosition

  def SeqDecl(mods: Mods, vals: Seq[String], tpe: TypeTree): DefTree =
    d.PatDef(mods, vals.map(n => d.Ident(n.toTermName)).toList, tpe, d.EmptyTree).withPosition

  def SecondaryCtor(mods: Mods, paramss: Seq[Seq[Param]], rhs: TermTree): DefTree =
    DefDef(mods, nme.CONSTRUCTOR.toString, Nil, paramss, Some(d.TypeTree()), rhs).withPosition

  // qual.T[A, B](x, y)(z)
  def InitCall(qual: Option[Tree], name: String, targs: Seq[TypeTree], argss: Seq[Seq[TermTree]]): InitCall = {
    val select = if (qual.isEmpty) d.Ident(name.toTermName) else d.Select(qual.get, name.toTypeName)
    val fun = if (targs.size == 0) select else TypeApply(select, targs.toList)
    ApplySeq(fun, argss).withPosition
  }

  def Param(mods: Mods, name: String, tpe: Option[TypeTree], default: Option[TermTree]): Param = {
    d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), getOrEmpty(default))
      .withMods(mods).withFlags(Flags.TermParam).withPosition.asInstanceOf[Param]
  }

  def TypeParam(mods: Mods, name: String, tparams: Seq[TypeParam], tboundsOpt: Option[TypeTree], cbounds: Seq[TypeTree]): TypeParam = {
    val tbounds = tboundsOpt.getOrElse(d.TypeBoundsTree(d.EmptyTree, d.EmptyTree)).asInstanceOf[d.TypeBoundsTree]
    val inner =
      if (cbounds.size == 0) tbounds
      else d.ContextBounds(tbounds, cbounds.toList.map(d.AppliedTypeTree(_, d.Ident(name.toTypeName))))

    val body =
      if (tparams.size == 0) inner
      else d.LambdaTypeTree(tparams.toList.asInstanceOf[List[d.TypeDef]], inner)

    d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
  }

  def Self(name: String, tpe: TypeTree): Self =
    d.ValDef(name.toTermName, tpe, d.EmptyTree).withPosition

  def Self(name: String): Self =
    d.ValDef(name.toTermName, d.TypeTree(), d.EmptyTree).withPosition

  // types
  def TypeIdent(name: String): TypeTree = d.Ident(name.toTypeName).withPosition

  def TypeSelect(qual: Tree, name: String): TypeTree = d.Select(qual, name.toTypeName).withPosition

  def TypeSingleton(ref: Tree): TypeTree = d.SingletonTypeTree(ref).withPosition

  def TypeApply(tpe: TypeTree, args: Seq[TypeTree]): TypeTree = d.AppliedTypeTree(tpe, args.toList).withPosition

  def TypeApplyInfix(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree = d.InfixOp(lhs, d.Ident(op.toTypeName), rhs).withPosition

  def TypeFunction(params: Seq[TypeTree], res: TypeTree): TypeTree = d.Function(params.toList, res).withPosition

  def TypeTuple(args: Seq[TypeTree]): TypeTree = d.Tuple(args.toList).withPosition

  def TypeAnd(lhs: TypeTree, rhs: TypeTree): TypeTree = d.AndTypeTree(lhs, rhs).withPosition

  def TypeOr(lhs: TypeTree, rhs: TypeTree): TypeTree = d.OrTypeTree(lhs, rhs).withPosition

  def TypeRefine(tpe: Option[TypeTree], stats: Seq[Tree]): TypeTree =
    d.RefinedTypeTree(tpe.getOrElse(d.EmptyTree), stats.toList).withPosition

  def TypeBounds(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree = {
    require(lo.nonEmpty || hi.nonEmpty)
    d.TypeBoundsTree(lo.getOrElse(d.EmptyTree), hi.getOrElse(d.EmptyTree)).withPosition
  }

  def TypeRepeated(tpe: TypeTree): TypeTree = d.PostfixOp(tpe, d.Ident(nme.raw.STAR)).withPosition

  def TypeByName(tpe: TypeTree): TypeTree = d.ByNameTypeTree(tpe).withPosition

  def TypeAnnotated(tpe: TypeTree, annots: Seq[Tree]): TypeTree = {
    require(annots.size > 0)
    annots.tail.foldRight(d.Annotated(tpe, annots.head).withPosition) { (ann: Tree, acc: TypeTree) =>
      d.Annotated(acc, ann).withPosition
    }
  }

  // terms
  def Lit(value: Any): TermTree = d.Literal(Constant(value)).withPosition

  def Ident(name: String): TermTree = d.Ident(name.toTermName).withPosition

  def Select(qual: TermTree, name: String): TermTree = d.Select(qual, name.toTermName).withPosition

  def This(qual: String): TermTree = d.This(d.Ident(qual.toTypeName)).withPosition

  def Super(thisp: String, superp: String): TermTree =
    d.Super(d.Ident(thisp.toTypeName), d.Ident(superp.toTypeName)).withPosition

  def Interpolate(prefix: String, parts: Seq[String], args: Seq[TermTree]): TermTree = {
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

  def Apply(fun: TermTree, args: Seq[TermTree]): TermTree = d.Apply(fun, args.toList).withPosition

  def ApplyType(fun: Tree, args: Seq[TypeTree]): TermTree = d.TypeApply(fun, args.toList).withPosition

  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  def Infix(lhs: TermTree, op: String, rhs: TermTree): TermTree =
    d.Apply(d.Select(lhs, op.toTermName), List(rhs)).withPosition

  def Prefix(op: String, od: TermTree): TermTree = d.PrefixOp(d.Ident(op.toTermName), od).withPosition

  def Postfix(od: TermTree, op: String): TermTree = d.PostfixOp(od, d.Ident(op.toTermName)).withPosition

  def Assign(lhs: TermTree, rhs: TermTree): TermTree = d.Assign(lhs, rhs).withPosition

  def Return(expr: TermTree): TermTree = d.Return(expr, d.EmptyTree).withPosition

  def Return: TermTree = d.Return(d.EmptyTree, d.EmptyTree).withPosition

  def Throw(expr: TermTree): TermTree = d.Throw(expr).withPosition

  def Ascribe(expr: TermTree, tpe: TypeTree): TermTree = d.Typed(expr, tpe).withPosition

  def Annotated(expr: TermTree, annots: Seq[Tree]): TermTree = {
    require(annots.size > 0)
    annots.tail.foldRight(d.Annotated(expr, annots.head).withPosition) { (ann: Tree, acc: TermTree) =>
      d.Annotated(acc, ann).withPosition
    }
  }

  def Tuple(args: Seq[TermTree]): TermTree = d.Tuple(args.toList).withPosition

  def Block(stats: Seq[Tree]): TermTree = {
    if (stats.size == 0)
      d.Block(stats.toList, d.EmptyTree).withPosition
    else
      d.Block(stats.init.toList, stats.last).withPosition
  }

  def If(cond: TermTree, thenp: TermTree, elsep: Option[TermTree]): TermTree =
    d.If(cond, thenp, elsep.getOrElse(d.EmptyTree)).withPosition

  def Match(expr: TermTree, cases: Seq[Tree]): TermTree =
    d.Match(expr, cases.toList.asInstanceOf[List[d.CaseDef]]).withPosition

  def Case(pat: TermTree, cond: Option[TermTree], body: TermTree): Tree =
    d.CaseDef(pat, cond.getOrElse(d.EmptyTree), body).withPosition

  def Try(expr: TermTree, cases: Seq[Tree], finallyp: Option[TermTree]): TermTree =
    d.Try(expr, cases.toList.asInstanceOf[List[d.CaseDef]], finallyp.getOrElse(d.EmptyTree)).withPosition

  def Try(expr: TermTree, handler: Tree, finallyp: Option[TermTree]): TermTree =
    d.ParsedTry(expr, handler, finallyp.getOrElse(d.EmptyTree)).withPosition

  def Function(params: Seq[Param], body: TermTree): TermTree =
    d.Function(params.toList, body).withPosition

  def PartialFunction(cases: Seq[Tree]): TermTree =
    d.Match(d.Thicket(Nil), cases.toList.asInstanceOf[List[d.CaseDef]]).withPosition

  def While(expr: TermTree, body: TermTree): TermTree = d.WhileDo(expr, body).withPosition

  def DoWhile(body: TermTree, expr: TermTree): TermTree = d.DoWhile(body, expr).withPosition

  def For(enums: Seq[Tree], body: TermTree): TermTree = ???

  def ForYield(enums: Seq[Tree], body: TermTree): TermTree = ???

  def GenFrom(pat: TermTree, rhs: TermTree): Tree = ???

  def GenAlias(pat: TermTree, rhs: TermTree): Tree = ???

  def Guard(cond: TermTree): Tree = ???

  // can be InitCall or AnonymClass
  def New(tpe: Tree): TermTree = d.New(tpe).withPosition

  def Named(name: String, expr: TermTree): TermTree =
    d.NamedArg(name.toTermName, expr).withPosition

  def Repeated(expr: TermTree): TermTree =
    d.Typed(expr, d.Ident(tpnme.WILDCARD_STAR)).withPosition

  // patterns
  def Bind(name: String, expr: TermTree): TermTree =
    d.Bind(name.toTermName, expr).withPosition

  def Alternative(trees: Seq[TermTree]): TermTree =
    d.Alternative(trees.toList).withPosition

  // importees
  def Import(items: Seq[Tree]): Tree =
    if (items.size == 1)
      items(0).withPosition
    else
      d.Thicket(items.toList).withPosition

  def ImportItem(ref: Tree, importees: Seq[Tree]): Tree =
    d.Import(ref, importees.toList).withPosition

  def ImportName(name: String): Tree = d.Ident(name.toTermName).withPosition

  def ImportRename(from: String, to: String): Tree =
    d.Thicket(d.Ident(from.toTermName), d.Ident(to.toTermName)).withPosition

  def ImportHide(name: String): Tree =
    d.Thicket(d.Ident(name.toTermName), d.Ident(nme.WILDCARD)).withPosition

  // extractors
  object Lit extends LitHelper {
    def unapply(tree: Tree): Option[Any] = tree match {
      case c.Literal(Constant(v)) => Some(v)
      case _ => None
    }
  }

  object Ident extends IdentHelper {
    def unapply(tree: Tree): Option[String] = tree match {
      case c.Ident(name) if name.isTermName => Some(name.show)
      case _ => None
    }
  }

  object This extends ThisHelper {
    def unapply(tree: Tree): Option[String] = tree match {
      case c.This(c.Ident(name)) => Some(name.show)
      case _ => None
    }
  }

  object Select extends SelectHelper {
    def unapply(tree: Tree): Option[(TermTree, String)] = tree match {
      case c.Select(qual, name) if name.isTermName => Some((qual, name.show))
      case _ => None
    }
  }

  object Apply extends ApplyHelper {
    def unapply(tree: Tree): Option[(TermTree, Seq[TermTree])] = tree match {
      case c.Apply(fun, Seq(c.Typed(c.SeqLiteral(args, _), _))) => Some((fun, args))
      case c.Apply(fun, args) => Some((fun, args))
      case _ => None
    }
  }

  object Ascribe extends AscribeHelper {
    def unapply(tree: Tree): Option[(TermTree, TypeTree)] = tree match {
      case c.Typed(expr, tpe) => Some((expr, tpe))
      case _ => None
    }
  }

  object Assign extends AssignHelper {
    def unapply(tree: Tree): Option[(TermTree, TermTree)] = tree match {
      case c.Assign(lhs, rhs) => Some((lhs, rhs))
      case _ => None
    }
  }

  object Annotated extends AnnotatedHelper {
    def unapply(tree: Tree): Option[(TermTree, Seq[Tree])] = {
      def recur(acc: Seq[Tree], term: Tree): (Tree, Seq[Tree])  = term match {
        case c.Annotated(expr, annot) => recur(annot +: acc, expr) // inner-most is in the front
        case expr => (expr, acc)
      }
      Some(recur(Nil, tree))
    }
  }

  object Block extends BlockHelper {
    def unapply(tree: Tree): Option[Seq[Tree]] = tree match {
      case c.Block(stats, expr) => Some(stats :+ expr)
      case _ => None
    }
  }

  object Tuple extends TupleHelper {
    def unapply(tree: Tree): Option[Seq[TermTree]] = tree match {
      case d.Tuple(trees) => Some(trees)
      case _ => None
    }
  }

}

class StructToolbox(enclosingPosition: Position)(implicit ctx: Context) extends Toolbox(enclosingPosition)(ctx) with STbox {

  object Object extends ObjectHelper {
    def unapply(tree: Tree): Option[(Mods, String, Seq[InitCall], Option[Self], Seq[Tree])] = tree match {
      case obj @ d.ModuleDef(name, templ @ c.Template(constr, parents, self: d.ValDef, body)) =>
        val selfOpt = if (self == d.EmptyValDef) None else Some(Self(self.name.toString, self.tpt))
        Some((obj.mods, name.toString, parents, selfOpt, templ.body))
      case _ => None
    }
  }

  object Class extends ClassHelper {
    def unapply(tree: Tree): Option[(Mods, String, Seq[TypeParam], Mods, Seq[Seq[Param]], Seq[InitCall], Option[Self], Seq[Tree])] = tree match {
      case cdef @ c.TypeDef(name, templ @ c.Template(constr, parents, self, body)) =>
        var tparams: List[TypeParam] = Nil
        val (paramss, cmods) = constr match {
          case c.DefDef(nme.CONSTRUCTOR, Nil, Nil, c.TypeTree(), d.EmptyTree) => (Nil, emptyMods)
          case pctor @ c.DefDef(nme.CONSTRUCTOR, tps, paramss, c.TypeTree(), d.EmptyTree) =>
            tparams = tps
            (paramss, pctor.mods : Mods)
        }
        val selfOpt = if (self == d.EmptyValDef) None else Some(self)
        Some((cdef.mods, name.toString, tparams, cmods, paramss, parents, selfOpt, templ.body))
      case _ => None
    }
  }

  object Trait extends TraitHelper {
    def unapply(tree: Tree): Option[(Mods, String, Seq[TypeParam], Mods, Seq[Seq[Param]], Seq[InitCall], Option[Self], Seq[Tree])] =
      if (!tree.isInstanceOf[d.TypeDef]) return None
      else {
        val typedef = tree.asInstanceOf[d.TypeDef]
        if (!typedef.mods.is(Flags.Trait)) return None

        Class.unapply(typedef)
      }
  }

  /*
  object Param extends ParamHelper {
    def unapply(tree: Tree): Option[(Mods, String, Option[TypeTree], Option[Tree])] = tree match {
      case valdef @ c.ValDef(name, tpe, _) =>
        val tpeOpt = if (tpe == d.TypeTree()) None else Some(tpe)
        val defaultOpt = if (valdef.rhs == d.EmptyTree) None else Some(valdef.rhs)
        Some((new Modifiers(valdef.mods), name.toString, tpeOpt, defaultOpt))
      case _ => None
    }
  }*/

   // accessors
  def toParamRep(tree: Param): ParamRep = new ParamRep {
    def mods: Mods = tree.mods
    def name: String = tree.name.show
    def tpt: Option[TypeTree] = if (tree.tpt.isInstanceOf[d.TypeTree]) None else Some(tree.tpt)
    def default: Option[TermTree] = if (tree.forceIfLazy == d.EmptyTree) None else Some(tree.forceIfLazy)
    def copy(
      name: String = this.name,
      mods: Mods = this.mods,
      tptOpt: Option[TypeTree] = this.tpt,
      defaultOpt: Option[TermTree] = this.default
    ): Param =
      d.cpy.ValDef(tree)(
        name.toTermName,
        tptOpt.getOrElse(d.TypeTree()),
        defaultOpt.getOrElse(d.EmptyTree)
      ).withMods(mods)
  }

  object ClassRep extends ClassRepHelper {
    def unapply(tree: Tree): Option[ClassRep] = tree match {
      case cdef @ c.TypeDef(name, c.Template(_, _, _, _)) => Some(toClassRep(cdef))
      case _ => None
    }
  }

  def toClassRep(tree: Class): ClassRep = tree match {
    case cdef@c.TypeDef(name, templ@c.Template(constr, parents, self, body)) =>
      var tparams1: List[TypeParam] = Nil
      val (paramss1, cmods) = constr match {
        case c.DefDef(nme.CONSTRUCTOR, Nil, Nil, c.TypeTree(), d.EmptyTree) => (Nil, emptyMods)
        case pctor@c.DefDef(nme.CONSTRUCTOR, tps, paramss, c.TypeTree(), d.EmptyTree) =>
          tparams1 = tps
          (paramss, pctor.mods: Mods)
      }
      val selfOpt = if (self == d.EmptyValDef) None else Some(self)

      new ClassRep {
        def mods: Mods = cdef.mods
        def ctorMods: Mods = cmods
        def name: String = name.toString
        def tparams: Seq[TypeParam] = tparams1
        def paramss: Seq[Seq[Param]] = paramss1
        def self: Option[Self] = selfOpt
        def stats: Seq[Tree] = templ.body

        def copy(mods: Mods, paramss: Seq[Seq[Param]], stats: Seq[Tree]): Class = {
          val tmpl1 = cdef.rhs.asInstanceOf[d.Template]
          val contr2 = d.cpy.DefDef(tmpl1.constr)(vparamss = paramss.toList.map(_.toList))
          val tmpl2 = d.cpy.Template(tmpl1)(constr = contr2)
          d.cpy.TypeDef(cdef)(rhs = tmpl1).withMods(mods)
        }
      }
  }
}

class TypeToolbox(enclosingPosition: Position)(implicit ctx: Context) extends Toolbox(enclosingPosition)(ctx) with TTbox {
  type Type = Types.Type
  type Member = Symbols.Symbol

  /** get the location where the def macro is used */
  def currentLocation: Location = Location(ctx.compilationUnit.source.file.name, enclosingPosition.line(), enclosingPosition.column())

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
  def caseFields(tp: Type): Seq[Member] = {
    val sym = tp.classSymbol.asClass
    if (!sym.is(Flags.Case)) return Nil

    sym.info.decls.filter(fd => fd.is(Flags.ParamAccessor)).toSeq
  }

  /* field with the given name */
  def field(tp: Type, name: String): Option[Member] = {
    val sym = tp.widen.classSymbol.asClass
    val denot = sym.info.memberExcluding(name.toTermName, Flags.Method)

    if (denot.exists) Some(denot.symbol)
    else None
  }

  /** name of a member */
  def name(mem: Member): String = mem.showName

  /** type of a member with respect to a prefix */
  def asSeenFrom(mem: Member, prefix: Type): Type = mem.asSeenFrom(prefix).info


  object SeqLiteral extends SeqLiteralHelper {
    def unapply(tree: Tree): Option[Seq[Tree]] = tree match {
      case c.Typed(c.SeqLiteral(elems,_), _) => Some(elems)
      case _ => None
    }
  }

  /*
  object ApplyType extends ApplyTypeHelper {
    def unapply(tree: Tree): Option[(Tree, Seq[TypeTree])] = tree match {
      case c.TypeApply(fun, args) => Some((fun, args))
      case _ => None
    }
  }

  object Assign extends AssignHelper {
    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case c.Assign(lhs, rhs) => Some((lhs, rhs))
      case _ => None
    }
  } */

}
