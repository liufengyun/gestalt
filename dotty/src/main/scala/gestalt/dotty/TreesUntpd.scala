package scala.gestalt.dotty

import gestalt.core

import dotty.tools.dotc
import dotc.core._
import dotc.ast.{Trees => c, tpd => t, untpd => d}
import StdNames._
import NameOps._
import Names.TermName
import Decorators._
import Constants._
import d.modsDeco
import dotc.typer.ProtoTypes._
import dotc.util.Positions.Position
import Contexts.Context


class Untpd(val toolbox: Toolbox) extends core.Untpd {
  import toolbox.types.Type
  import toolbox.{ tpd, enclosingPosition, Unsafe, Position }

  implicit val ctx: Contexts.Context = toolbox.ctx

  type Tree = d.Tree
  type TypeTree = d.Tree
  type TermTree = d.Tree
  type DefTree = d.Tree
  type PatTree = d.Tree

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

  // modifiers
  def emptyMods: Mods = DottyModifiers(d.EmptyModifiers)

  implicit  def toDotty(tbMods: Mods): d.Modifiers = tbMods.dottyMods

  implicit def fromDotty(dottyMods: d.Modifiers): Mods = DottyModifiers(dottyMods)

  /*------------------------------ helpers ------------------------------*/
  def getOrEmpty(treeOpt: Option[Tree]): Tree = treeOpt.getOrElse(d.EmptyTree)

  implicit class TreeHelper(tree: d.Tree) {
    def withPosition[T <: Tree] = tree.withPos(enclosingPosition).asInstanceOf[T]
  }

  def ApplySeq(fun: TermTree, argss: List[List[TermTree]]): TermTree =
    argss.foldLeft(fun) { (acc, args) => d.Apply(acc, args) }

  /*------------------------------- type trees -----------------------------------*/

  def TypeIdent(name: String)(implicit unsafe: Unsafe): TypeTree =
    d.Ident(name.toTypeName).withPosition

  def TypeSelect(qual: Tree, name: String): TypeTree =
    d.Select(qual, name.toTypeName).withPosition

  def TypeSingleton(ref: Tree): TypeTree =
    d.SingletonTypeTree(ref).withPosition

  def TypeApply(tpe: TypeTree, args: List[TypeTree]): TypeTree =
    d.AppliedTypeTree(tpe, args).withPosition

  def TypeInfix(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree =
    d.InfixOp(lhs, d.Ident(op.toTypeName), rhs).withPosition

  def TypeFunction(params: List[TypeTree], res: TypeTree): TypeTree =
    d.Function(params, res).withPosition

  def TypeTuple(args: List[TypeTree]): TypeTree =
    d.Tuple(args).withPosition

  def TypeAnd(lhs: TypeTree, rhs: TypeTree): TypeTree =
    d.AndTypeTree(lhs, rhs).withPosition

  def TypeOr(lhs: TypeTree, rhs: TypeTree): TypeTree =
    d.OrTypeTree(lhs, rhs).withPosition

  def TypeRefine(stats: List[Tree]): TypeTree =
    d.RefinedTypeTree(d.EmptyTree, stats).withPosition
  def TypeRefine(tpe: TypeTree, stats: List[Tree]): TypeTree =
    d.RefinedTypeTree(tpe, stats).withPosition

  def TypeBounds(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree = {
    require(lo.nonEmpty || hi.nonEmpty)
    d.TypeBoundsTree(lo.getOrElse(d.EmptyTree), hi.getOrElse(d.EmptyTree)).withPosition
  }

  def TypeRepeated(tpe: TypeTree): TypeTree =
    d.PostfixOp(tpe, d.Ident(nme.raw.STAR)).withPosition

  def TypeByName(tpe: TypeTree): TypeTree =
    d.ByNameTypeTree(tpe).withPosition

  def TypeAnnotated(tpe: TypeTree, annots: List[Tree]): TypeTree = {
    require(annots.size > 0)
    annots.tail.foldRight(d.Annotated(tpe, annots.head).withPosition) { (ann: Tree, acc: TypeTree) =>
      d.Annotated(acc, ann).withPosition[TypeTree]
    }
  }


  /*------------------------------- terms-------------------------------------*/

  def NewAnonymClass(parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): TermTree = {
    val init = d.DefDef(nme.CONSTRUCTOR, List(), List(), d.TypeTree(), d.EmptyTree)
    val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
    d.New(d.Template(init, parents, self, stats).withPosition)
  }

  def NewInstance(typeTree: TypeTree, argss: List[List[TermTree]]): TermTree =
    ApplySeq(d.Select(d.New(typeTree), nme.CONSTRUCTOR), argss).withPosition

  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  def Infix(lhs: TermTree, op: String, rhs: TermTree): TermTree =
    d.InfixOp(lhs, d.Ident(op.toTermName), rhs).withPosition

  def Prefix(op: String, od: TermTree): TermTree =
    d.PrefixOp(d.Ident(op.toTermName), od).withPosition

  def Postfix(od: TermTree, op: String): TermTree =
    d.PostfixOp(od, d.Ident(op.toTermName)).withPosition

  def Throw(expr: TermTree): TermTree = d.Throw(expr).withPosition

  def Annotated(expr: TermTree, annots: List[Tree]): TermTree = {
    require(annots.size > 0)
    annots.tail.foldRight(d.Annotated(expr, annots.head).withPosition) { (ann: Tree, acc: TermTree) =>
      d.Annotated(acc, ann).withPosition[TermTree]
    }
  }

  def If(cond: TermTree, thenp: TermTree, elsep: Option[TermTree]): TermTree =
    d.If(cond, thenp, elsep.getOrElse(d.EmptyTree)).withPosition

  def Try(expr: Tree, cases: List[Tree], finallyp: Option[TermTree]): TermTree =
    d.Try(expr, cases.asInstanceOf[List[d.CaseDef]], finallyp.getOrElse(d.EmptyTree)).withPosition
  def Try(expr: Tree, handler: Tree, finallyp: Option[TermTree]): TermTree =
    d.ParsedTry(expr, handler, finallyp.getOrElse(d.EmptyTree)).withPosition


  def Function(params: List[Param], body: TermTree): TermTree =
    d.Function(params, body).withPosition

  def While(cond: TermTree, body: TermTree): TermTree = d.WhileDo(cond, body).withPosition

  def DoWhile(body: TermTree, cond: TermTree): TermTree =
    d.DoWhile(body, cond).withPosition

  object For extends ForImpl {
    def ForDo(enums: List[Tree], body: TermTree): TermTree = d.ForDo(enums, body)
    def ForYield(enums: List[Tree], body: TermTree): TermTree = d.ForYield(enums, body)
    def GenFrom(pat: PatTree, rhs: TermTree): Tree = d.GenFrom(pat, rhs)
    def GenAlias(pat: PatTree, rhs: TermTree): Tree = d.GenAlias(pat, rhs)
    def Guard(cond: TermTree): Tree = cond
  }

  // named arguments
  def Named(name: String, expr: Tree): TermTree =
    d.NamedArg(name.toTermName, expr).withPosition

  def Repeated(expr: Tree): TermTree =
    d.Typed(expr, d.Ident(tpnme.WILDCARD_STAR)).withPosition

  def Apply(fun: TermTree, args: List[TermTree]): TermTree =
    d.Apply(fun, args).withPosition

  def ApplyType(fun: TermTree, args: List[TypeTree]): TermTree =
    d.TypeApply(fun, args).withPosition

  def Ident(name: String)(implicit unsafe: Unsafe): TermTree =
    d.Ident(name.toTermName).withPosition

  def Lit(value: Any): TermTree = t.Literal(Constant(value)).withPosition


  def This(qual: String): TermTree = d.This(d.Ident(qual.toTypeName)).withPosition

  def Super(thisp: String, superp: String): TermTree =
    d.Super(d.Ident(thisp.toTypeName), d.Ident(superp.toTypeName)).withPosition

  def Select(qual: TermTree, name: String): TermTree =
    d.Select(qual, name.toTermName).withPosition

  def Ascribe(expr: TermTree, tpe: TypeTree): TermTree =
    d.Typed(expr, tpe).withPosition

  def Assign(lhs: TermTree, rhs: TermTree): TermTree =
    d.Assign(lhs, rhs).withPosition

  def Update(fun: TermTree, argss: List[List[TermTree]], rhs: TermTree): TermTree =
    d.Assign(ApplySeq(fun, argss), rhs)

  def Return(expr: TermTree): TermTree = d.Return(expr, d.EmptyTree).withPosition
  def Return(): TermTree = d.Return(d.EmptyTree, d.EmptyTree).withPosition

  def Block(stats: List[Tree]): TermTree = {
    if (stats.size == 0)
      d.Block(stats, d.EmptyTree).withPosition
    else
      d.Block(stats.init, stats.last).withPosition
  }

  def PartialFunction(cases: List[Tree]): TermTree =
    d.Match(d.EmptyTree, cases.asInstanceOf[List[d.CaseDef]]).withPosition

  def Match(expr: TermTree, cases: List[Tree]): TermTree =
    d.Match(expr, cases.asInstanceOf[List[d.CaseDef]]).withPosition

  def Case(pat: PatTree, cond: Option[TermTree], body: TermTree): Tree =
    d.CaseDef(pat, cond.getOrElse(d.EmptyTree), body).withPosition

  def Tuple(args: List[TermTree]): TermTree =
    d.Tuple(args).withPosition

  def Interpolate(prefix: String, parts: List[String], args: List[TermTree]): TermTree = {
    val thickets =
      for {(arg, part) <- args.zip(parts.take(args.size))}
        yield {
          val expr = arg match {
            case tree: d.Ident => tree
            case tree => d.Block(Nil, tree)
          }
          d.Thicket(d.Literal(Constant(part)), expr)
        }
    val segments =
      if (parts.size > args.size)
        thickets :+ d.Literal(Constant(parts.last))
      else thickets

    d.InterpolatedString(prefix.toTermName, segments).withPosition
  }

  // patterns
  object Pat extends PatImpl {
    def Bind(name: String, expr: PatTree): PatTree =
      d.Bind(name.toTermName, expr).withPosition

    def Alt(trees: List[PatTree]): PatTree =
      d.Alternative(trees).withPosition

    def Lit(v: Any): PatTree =
      d.Literal(Constant(v)).withPosition

    def Ident(name: String): PatTree =
      d.Ident(name.toTermName)

    def Var(name: String): PatTree =
      d.Ident(name.toTermName).withPosition

    def Ascribe(name: String, tp: TypeTree): PatTree =
      d.Typed(d.Ident(name.toTermName), tp).withPosition

    def Unapply(fun: TermTree, args: List[PatTree]): PatTree =
      d.Apply(fun, args).withPosition

    def Infix(lhs: PatTree, op: String, rhs: PatTree): PatTree =
      d.InfixOp(lhs, d.Ident(op.toTermName), rhs).withPosition

    def Tuple(pats: List[PatTree]): PatTree =
      d.Tuple(pats)
  }


  // importees
  object Import extends ImportImpl {
    def apply(items: List[Tree]): Tree =
      if (items.size == 1)
        items.head.withPosition
      else
        d.Thicket(items).withPosition

    def Item(ref: Tree, importees: List[Tree]): Tree =
      d.Import(ref, importees).withPosition

    def Name(name: String): Tree = d.Ident(name.toTermName).withPosition

    def Rename(from: String, to: String): Tree =
      d.Thicket(d.Ident(from.toTermName), d.Ident(to.toTermName)).withPosition

    def Hide(name: String): Tree =
      d.Thicket(d.Ident(name.toTermName), d.Ident(nme.WILDCARD)).withPosition
  }

  /*------------------------------- definitions-------------------------------------*/

  def ValDef(mods: Mods, name: String, tpe: Option[TypeTree], rhs: Tree): ValDef =
    d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), rhs).withMods(mods).withPosition

  def ValDecl(mods: Mods, name: String, tpe: TypeTree): ValDecl =
    d.ValDef(name.toTermName, tpe, d.EmptyTree).withMods(mods).withPosition

  def DefDef(mods: Mods, name: String, tparams: List[TypeParam], paramss: List[List[Param]], tpe: Option[TypeTree], rhs: Tree): DefDef =
    d.DefDef(name.toTermName, tparams, paramss, tpe.getOrElse(d.TypeTree()), rhs).withMods(mods).withPosition

  def DefDecl(mods: Mods, name: String, tparams: List[TypeParam], paramss: List[List[Param]], tpe: TypeTree): DefDecl =
    d.DefDef(name.toTermName, tparams, paramss, tpe, d.EmptyTree).withMods(mods).withPosition

  def Param(mods: Mods, name: String, tpe: Option[TypeTree], default: Option[TermTree]): Param = {
    d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), getOrEmpty(default))
      .withMods(mods.dottyMods | Flags.TermParam).withPosition
  }

  def TypeParam(mods: Mods, name: String, tparams: List[TypeParam], tboundsOpt: Option[TypeTree], cbounds: List[TypeTree]): TypeParam = {
    val tbounds = tboundsOpt.getOrElse(d.TypeBoundsTree(d.EmptyTree, d.EmptyTree)).asInstanceOf[d.TypeBoundsTree]
    val inner =
      if (cbounds.size == 0) tbounds
      else d.ContextBounds(tbounds, cbounds.map(d.AppliedTypeTree(_, d.Ident(name.toTypeName))))

    val body =
      if (tparams.size == 0) inner
      else d.LambdaTypeTree(tparams, inner)

    d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
  }

  def TypeDecl(mods: Mods, name: String, tparams: List[TypeParam], tboundsOpt: Option[TypeTree]): DefTree = {
    val tbounds = tboundsOpt.getOrElse(d.TypeBoundsTree(d.EmptyTree, d.EmptyTree))
    val body =
      if (tparams.size == 0) tbounds
      else d.LambdaTypeTree(tparams.asInstanceOf[List[d.TypeDef]], tbounds)
    d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
  }

  def TypeAlias(mods: Mods, name: String, tparams: List[TypeParam], rhs: TypeTree): DefTree = {
    val body =
      if (tparams.size == 0) rhs
      else d.LambdaTypeTree(tparams.asInstanceOf[List[d.TypeDef]], rhs)
    d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
  }

  def PatDef(mods: Mods, lhs: PatTree, tpe: Option[TypeTree], rhs: Tree): DefTree =
    d.PatDef(mods, List(lhs), tpe.getOrElse(d.TypeTree()), rhs).withPosition

  // extends qual.T[A, B](x, y)(z)
  def InitCall(tpe: TypeTree, argss: List[List[TermTree]]): InitCall =
    ApplySeq(tpe, argss).withPosition

  def Self(name: String, tpe: TypeTree): Self =
    d.ValDef(name.toTermName, tpe, d.EmptyTree).withPosition
  def Self(name: String): Self =
    d.ValDef(name.toTermName, d.TypeTree(), d.EmptyTree).withPosition


  def Class(mods: Mods, name: String, tparams: List[TypeParam], ctorMods: Mods, paramss: List[List[Param]], parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Class = {
    val constr =
      if (paramss.isEmpty) d.DefDef(nme.CONSTRUCTOR, Nil, Nil, d.TypeTree(), d.EmptyTree)
      else {
        d.DefDef(nme.CONSTRUCTOR, tparams, paramss, d.TypeTree(), d.EmptyTree).withMods(ctorMods)
      }

    val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
    val templ = d.Template(constr, parents, self, stats)
    d.TypeDef(name.toTypeName, templ).withMods(mods).withPosition
  }

  def Trait(mods: Mods, name: String, tparams: List[TypeParam], ctorMods: Mods, paramss: List[List[Param]], parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Trait =
    Class(mods.dottyMods | Flags.Trait, name, tparams, ctorMods, paramss, parents, selfOpt, stats).withPosition


  def Object(mods: Mods, name: String, parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Object = {
    val constr = d.DefDef(nme.CONSTRUCTOR, Nil, Nil, d.TypeTree(), d.EmptyTree)
    val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
    val templ = d.Template(constr, parents, self, stats)
    d.ModuleDef(name.toTermName, templ).withMods(mods).withPosition
  }

  /*------------------------------- TreeOps-------------------------------------*/
  def pos(tree: Tree): Position = tree.pos

  def show(tree: Tree): String = tree.show
}
