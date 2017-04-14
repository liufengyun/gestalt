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
  type Mods = DottyModifiers

  case class DottyModifiers(dottyMods: d.Modifiers) extends Modifiers {
    def isPrivate: Boolean = dottyMods.is(Flags.Private)
    def isProtected: Boolean = dottyMods.is(Flags.Protected)
    def isOverride: Boolean = dottyMods.is(Flags.Override)
    def isFinal: Boolean = dottyMods.is(Flags.Final)
    def isImplicit: Boolean = dottyMods.is(Flags.Implicit)
    def isLazy: Boolean = dottyMods.is(Flags.Lazy)
    def isSealed: Boolean = dottyMods.is(Flags.Sealed)
    def isAbstract: Boolean = dottyMods.is(Flags.Abstract)
    def isMutable: Boolean = dottyMods.is(Flags.Mutable)
    def isCase: Boolean = dottyMods.is(Flags.Case)
    def isContravariant: Boolean = dottyMods.is(Flags.Contravariant)
    def isCovariant: Boolean = dottyMods.is(Flags.Covariant)
    def isInline: Boolean = dottyMods.is(Flags.Inline)

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
    def setMutable: Mods = DottyModifiers(dottyMods | Flags.Mutable)
    def setCase: Mods = DottyModifiers(dottyMods | Flags.Case)
    def setContravariant: Mods = DottyModifiers(dottyMods | Flags.Contravariant)
    def setCovariant: Mods = DottyModifiers(dottyMods | Flags.Covariant)
    def setInline: Mods = DottyModifiers(dottyMods | Flags.Inline)

    def withAddedAnnotation(annot: d.Tree): Mods = DottyModifiers(dottyMods.withAddedAnnotation(annot))

    def hasAnnotations: Boolean = dottyMods.hasAnnotations

    def privateWithin: String =
      if (dottyMods.is(Flags.Local)) "this"
      else dottyMods.privateWithin.toString
  }

  // modifiers
  def emptyMods: Mods = DottyModifiers(d.EmptyModifiers)

  def fresh(prefix: String = "$local"): String = ctx.freshName(prefix)

  def getOrEmpty(treeOpt: Option[Tree]): Tree = treeOpt.getOrElse(d.EmptyTree)

  implicit class TreeHelper(tree: d.Tree) {
    def withPosition = tree.withPos(enclosingPosition)
  }

  // diagnostics - the implementation takes the position from the tree
  def error(message: String, tree: Tree): Unit = {
    ctx.error(message, tree.pos)
  }

  implicit  def toDotty(tbMods: Mods): d.Modifiers = tbMods.dottyMods

  implicit def fromDotty(dottyMods: d.Modifiers): Mods = DottyModifiers(dottyMods)

  //----------------------------------------

  def Object(mods: Mods, name: String, parents: Seq[Tree], selfOpt: Option[Tree], stats: Seq[Tree]): Tree = {
    val constr = d.DefDef(nme.CONSTRUCTOR, Nil, Nil, d.TypeTree(), d.EmptyTree)
    val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
    val templ = d.Template(constr, parents.toList, self, stats)
    d.ModuleDef(name.toTermName, templ).withMods(mods).withPosition
  }

  def Class(mods: Mods, name: String, tparams: Seq[Tree], ctor: Option[Tree], parents: Seq[Tree], selfOpt: Option[Tree], stats: Seq[Tree]): Tree = {
    val constr =
      if (ctor.isEmpty) d.DefDef(nme.CONSTRUCTOR, Nil, Nil, d.TypeTree(), d.EmptyTree)
      else {
        val PrimaryCtorTree(mods, paramss) = ctor.get
        val tparamsCast = tparams.toList.asInstanceOf[List[d.TypeDef]]
        val paramssCast = paramss.map(_.toList).toList.asInstanceOf[List[List[d.ValDef]]]
        d.DefDef(nme.CONSTRUCTOR, tparamsCast, paramssCast, d.TypeTree(), d.EmptyTree).withMods(mods)
      }

    val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
    val templ = d.Template(constr, parents.toList, self, stats)
    d.TypeDef(name.toTypeName, templ).withMods(mods).withPosition
  }

  def AnonymClass(parents: Seq[Tree], selfOpt: Option[Tree], stats: Seq[Tree]): Tree = {
    val init = d.DefDef(nme.CONSTRUCTOR, List(), List(), d.TypeTree(), d.EmptyTree)
    val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
    d.Template(init, parents.toList, self, stats).withPosition
  }

  def Trait(mods: Mods, name: String, tparams: Seq[Tree], ctor: Option[Tree], parents: Seq[Tree], self: Option[Tree], stats: Seq[Tree]): Tree =
    Class(mods, name, tparams, ctor, parents, self, stats).asInstanceOf[d.TypeDef].withFlags(Flags.Trait).withPosition

  def TypeDecl(mods: Mods, name: String, tparams: Seq[Tree], tboundsOpt: Option[TypeTree]): Tree = {
    val tbounds = tboundsOpt.getOrElse(d.TypeBoundsTree(d.EmptyTree, d.EmptyTree))
    val body =
      if (tparams.size == 0) tbounds
      else d.LambdaTypeTree(tparams.toList.asInstanceOf[List[d.TypeDef]], tbounds)
    d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
  }

  def TypeAlias(mods: Mods, name: String, tparams: Seq[Tree], rhs: TypeTree): Tree = {
    val body =
      if (tparams.size == 0) rhs
      else d.LambdaTypeTree(tparams.toList.asInstanceOf[List[d.TypeDef]], rhs)
    d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
  }

  def DefDef(mods: Mods, name: String, tparams: Seq[Tree], paramss: Seq[Seq[Tree]], tpe: Option[TypeTree], rhs: Tree): Tree = {
    val types = tparams.toList.asInstanceOf[List[d.TypeDef]]
    val params = paramss.map(_.toList).toList.asInstanceOf[List[List[d.ValDef]]]
    d.DefDef(name.toTermName, types, params, tpe.getOrElse(d.TypeTree()), rhs).withMods(mods).withPosition
  }

  def DefDecl(mods: Mods, name: String, tparams: Seq[Tree], paramss: Seq[Seq[Tree]], tpe: TypeTree): Tree = {
    val types = tparams.toList.asInstanceOf[List[d.TypeDef]]
    val params = paramss.map(_.toList).toList.asInstanceOf[List[List[d.ValDef]]]
    d.DefDef(name.toTermName, types, params, tpe, d.EmptyTree).withMods(mods).withPosition
  }

  def ValDef(mods: Mods, name: String, tpe: Option[TypeTree], rhs: Tree): Tree =
    d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), rhs).withMods(mods).withPosition

  def ValDef(mods: Mods, lhs: Tree, tpe: Option[TypeTree], rhs: Tree): Tree =
    d.PatDef(mods, List(lhs), tpe.getOrElse(d.TypeTree()), rhs).withPosition

  def ValDef(mods: Mods, pats: Seq[Tree], tpe: Option[TypeTree], rhs: Tree): Tree =
    d.PatDef(mods, pats.toList, tpe.getOrElse(d.TypeTree()), rhs).withPosition

  def ValDecl(mods: Mods, name: String, tpe: TypeTree): Tree =
    d.ValDef(name.toTermName, tpe, d.EmptyTree).withMods(mods).withPosition

  def ValDecl(mods: Mods, vals: Seq[String], tpe: TypeTree): Tree =
    d.PatDef(mods, vals.map(n => d.Ident(n.toTermName)).toList, tpe, d.EmptyTree).withPosition

  def VarDef(mods: Mods, name: String, tpe: Option[TypeTree], rhs: Tree): Tree =
    d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), rhs).withMods((mods : d.Modifiers) | Flags.Mutable).withPosition

  def VarDef(mods: Mods, lhs: Tree, tpe: Option[TypeTree], rhs: Tree): Tree =
    d.PatDef((mods: d.Modifiers) | Flags.Mutable, List(lhs), tpe.getOrElse(d.TypeTree()), rhs).withPosition

  def VarDef(mods: Mods, pats: Seq[Tree], tpe: Option[TypeTree], rhs: Tree): Tree =
    d.PatDef((mods: d.Modifiers) | Flags.Mutable, pats.toList, tpe.getOrElse(d.TypeTree()), rhs).withPosition

  def VarDecl(mods: Mods, name: String, tpe: TypeTree): Tree =
    d.ValDef(name.toTermName, tpe, d.EmptyTree).withMods((mods: d.Modifiers) | Flags.Mutable).withPosition

  def VarDecl(mods: Mods, vals: Seq[String], tpe: TypeTree): Tree =
    d.PatDef((mods: d.Modifiers)| Flags.Mutable, vals.map(n => d.Ident(n.toTermName)).toList, tpe, d.EmptyTree).withPosition

  // Dummy trees to retrofit Dotty AST
  protected case class PrimaryCtorTree(mods: Mods, paramss: Seq[Seq[Tree]]) extends d.Tree
  def PrimaryCtor(mods: Mods, paramss: Seq[Seq[Tree]]): Tree = PrimaryCtorTree(mods, paramss).withPosition

  def SecondaryCtor(mods: Mods, paramss: Seq[Seq[Tree]], rhs: Tree): Tree =
    DefDef(mods, nme.CONSTRUCTOR.toString, Nil, paramss, Some(d.TypeTree()), rhs).withPosition

  // qual.T[A, B](x, y)(z)
  def InitCall(qual: Option[Tree], name: String, tparams: Seq[TypeTree], argss: Seq[Seq[Tree]]): Tree = {
    val select = if (qual.isEmpty) d.Ident(name.toTermName) else d.Select(qual.get, name.toTypeName)
    val fun = if (tparams.size == 0) select else TypeApply(select, tparams.toList)
    ApplySeq(fun, argss).withPosition
  }

  def Param(mods: Mods, name: String, tpe: Option[TypeTree], default: Option[Tree]): Tree = {
    d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), getOrEmpty(default)).withMods(mods).withFlags(Flags.TermParam).withPosition
  }

  def TypeParam(mods: Mods, name: String, tparams: Seq[TypeTree], tboundsOpt: Option[TypeTree], cbounds: Seq[TypeTree]): TypeTree = {
    val tbounds = tboundsOpt.getOrElse(d.TypeBoundsTree(d.EmptyTree, d.EmptyTree)).asInstanceOf[d.TypeBoundsTree]
    val inner =
      if (cbounds.size == 0) tbounds
      else d.ContextBounds(tbounds, cbounds.toList.map(d.AppliedTypeTree(_, d.Ident(name.toTypeName))))

    val body =
      if (tparams.size == 0) inner
      else d.LambdaTypeTree(tparams.toList.asInstanceOf[List[d.TypeDef]], inner)

    d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
  }

  def Self(name: String, tpe: TypeTree): Tree =
    d.ValDef(name.toTermName, tpe, d.EmptyTree).withPosition

  def Self(name: String): Tree =
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
    annots.tail.foldRight(d.Annotated(tpe, annots.head).withPosition) { (ann, acc) =>
      d.Annotated(acc, ann).withPosition
    }
  }

  // terms
  def Lit(value: Any): Tree = d.Literal(Constant(value)).withPosition

  def Ident(name: String): Tree = d.Ident(name.toTermName).withPosition

  def Select(qual: Tree, name: String): Tree = d.Select(qual, name.toTermName).withPosition

  def This(qual: String): Tree = d.This(d.Ident(qual.toTypeName)).withPosition

  def Super(thisp: String, superp: String): Tree =
    d.Super(d.Ident(thisp.toTypeName), d.Ident(superp.toTypeName)).withPosition

  def Interpolate(prefix: String, parts: Seq[String], args: Seq[Tree]): Tree = {
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

  def Apply(fun: Tree, args: Seq[Tree]): Tree = d.Apply(fun, args.toList).withPosition

  def ApplyType(fun: Tree, args: Seq[TypeTree]): Tree = d.TypeApply(fun, args.toList).withPosition

  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  def Infix(lhs: Tree, op: String, rhs: Tree): Tree =
    d.Apply(d.Select(lhs, op.toTermName), List(rhs)).withPosition

  def Prefix(op: String, od: Tree): Tree = d.PrefixOp(d.Ident(op.toTermName), od).withPosition

  def Postfix(od: Tree, op: String): Tree = d.PostfixOp(od, d.Ident(op.toTermName)).withPosition

  def Assign(lhs: Tree, rhs: Tree): Tree = d.Assign(lhs, rhs).withPosition

  def Return(expr: Tree): Tree = d.Return(expr, d.EmptyTree).withPosition

  def Throw(expr: Tree): Tree = d.Throw(expr).withPosition

  def Ascribe(expr: Tree, tpe: Tree): Tree = d.Typed(expr, tpe).withPosition

  def Annotated(expr: Tree, annots: Seq[Tree]): Tree = {
    require(annots.size > 0)
    annots.tail.foldRight(d.Annotated(expr, annots.head).withPosition) { (ann, acc) =>
      d.Annotated(acc, ann).withPosition
    }
  }

  def Tuple(args: Seq[Tree]): Tree = d.Tuple(args.toList).withPosition

  def Block(stats: Seq[Tree]): Tree = {
    if (stats.size == 0)
      d.Block(stats.toList, d.EmptyTree).withPosition
    else
      d.Block(stats.init.toList, stats.last).withPosition
  }

  def If(cond: Tree, thenp: Tree, elsep: Option[Tree]): Tree =
    d.If(cond, thenp, elsep.getOrElse(d.EmptyTree)).withPosition

  def Match(expr: Tree, cases: Seq[Tree]): Tree =
    d.Match(expr, cases.toList.asInstanceOf[List[d.CaseDef]]).withPosition

  def Case(pat: Tree, cond: Option[Tree], body: Tree): Tree =
    d.CaseDef(pat, cond.getOrElse(d.EmptyTree), body).withPosition

  def Try(expr: Tree, cases: Seq[Tree], finallyp: Option[Tree]): Tree =
    d.Try(expr, cases.toList.asInstanceOf[List[d.CaseDef]], finallyp.getOrElse(d.EmptyTree)).withPosition

  def Try(expr: Tree, handler: Tree, finallyp: Option[Tree]): Tree =
    d.ParsedTry(expr, handler, finallyp.getOrElse(d.EmptyTree)).withPosition

  def Function(params: Seq[Tree], body: Tree): Tree =
    d.Function(params.toList, body).withPosition

  def PartialFunction(cases: Seq[Tree]): Tree =
    d.Match(d.Thicket(Nil), cases.toList.asInstanceOf[List[d.CaseDef]]).withPosition

  def While(expr: Tree, body: Tree): Tree = d.WhileDo(expr, body).withPosition

  def DoWhile(body: Tree, expr: Tree): Tree = d.DoWhile(body, expr).withPosition

  def For(enums: Seq[Tree], body: Tree): Tree = ???

  def GenFrom(pat: Tree, rhs: Tree): Tree = ???

  def GenAlias(pat: Tree, rhs: Tree): Tree = ???

  def Guard(cond: Tree): Tree = ???

  def Yield(expr: Tree): Tree = ???

  // can be InitCall or AnonymClass
  def New(tpe: Tree): Tree = d.New(tpe).withPosition

  def Named(name: String, expr: Tree): Tree =
    d.NamedArg(name.toTermName, expr).withPosition

  def Repeated(expr: Tree): Tree =
    d.Typed(expr, d.Ident(tpnme.WILDCARD_STAR)).withPosition

  // patterns
  def Bind(name: String, expr: Tree): Tree =
    d.Bind(name.toTermName, expr).withPosition

  def Alternative(lhs: Tree, rhs: Tree): Tree =
    d.Alternative(List(lhs, rhs)).withPosition

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

}

class StructToolbox(enclosingPosition: Position)(implicit ctx: Context) extends Toolbox(enclosingPosition)(ctx) with STbox {

  object Object extends ObjectHelper {
    def unapply(tree: Tree): Option[(Mods, String, Seq[Tree], Option[Tree], Seq[Tree])] = tree match {
      case obj @ d.ModuleDef(name, templ @ c.Template(constr, parents, self: d.ValDef, body)) =>
        val selfOpt = if (self == d.EmptyValDef) None else Some(Self(self.name.toString, self.tpt))
        Some((obj.mods, name.toString, parents, selfOpt, templ.body))
      case _ => None
    }
  }

  object Class extends ClassHelper {
    def unapply(tree: Tree): Option[(Mods, String, Seq[Tree], Option[Tree], Seq[Tree], Option[Tree], Seq[Tree])] = tree match {
      case cdef @ c.TypeDef(name, templ @ c.Template(constr, parents, self, body)) =>
        var tparams: List[Tree] = Nil
        val ctor = constr match {
          case c.DefDef(nme.CONSTRUCTOR, Nil, Nil, c.TypeTree(), d.EmptyTree) => None
          case pctor @ c.DefDef(nme.CONSTRUCTOR, tps, paramss, c.TypeTree(), d.EmptyTree) =>
            tparams = tps
            Some(PrimaryCtor(pctor.mods, paramss))
        }
        val selfOpt = if (self == d.EmptyValDef) None else Some(Self(self.name.toString, self.tpt))
        Some((cdef.mods, name.toString, tparams, ctor, tparams, selfOpt, templ.body))  // TODO: parents
      case _ => None
    }
  }

  object Trait extends TraitHelper {
    def unapply(tree: Tree): Option[(Mods, String, Seq[Tree], Option[Tree], Seq[Tree], Option[Tree], Seq[Tree])] =
      if (!tree.isInstanceOf[d.TypeDef]) return None
      else {
        val typedef = tree.asInstanceOf[d.TypeDef]
        if (!typedef.mods.is(Flags.Trait)) return None

        Class.unapply(typedef)
      }
  }

  object PrimaryCtor extends PrimaryCtorHelper {
    def unapply(tree: Tree): Option[(Mods, Seq[Seq[Tree]])] = tree match {
      case PrimaryCtorTree(mods, paramss) => Some((mods, paramss))
      case _                              => None
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

  object Ascribe extends AscribeHelper {
    def unapply(tree: Tree): Option[(Tree, TypeTree)] = tree match {
      case c.Typed(expr, tpt) => Some((expr, tpt))
      case _ => None
    }
  }

  object Lit extends LitHelper {
    def unapply(tree: Tree): Option[Any] = tree match {
      case c.Literal(Constant(v)) => Some(v)
      case _ => None
    }
  }

  object Apply extends ApplyHelper {
    def unapply(tree: Tree): Option[(Tree, Seq[Tree])] = tree match {
      case c.Apply(fun, Seq(c.Typed(c.SeqLiteral(args, _), _))) => Some((fun, args))
      case c.Apply(fun, args) => Some((fun, args))
      case _ => None
    }
  }

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
