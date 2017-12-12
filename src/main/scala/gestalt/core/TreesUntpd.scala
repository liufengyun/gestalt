package scala.gestalt.core

trait Untpd {
  val toolbox: Toolbox
  import toolbox._

  type Tree     >: Null <: AnyRef
  type TypeTree >: Null <: Tree
  type TermTree >: Null <: Tree
  type DefTree  >: Null <: Tree
  type PatTree  >: Null <: Tree

  type Class     <: DefTree
  type Trait     <: DefTree
  type Object    <: DefTree
  type Param     <: DefTree
  type TypeParam <: DefTree
  type ValDef    <: DefTree
  type ValDecl   <: DefTree
  type DefDef    <: DefTree
  type DefDecl   <: DefTree
  type Self      <: DefTree
  type InitCall  <: Tree

  type Splice   <: TypeTree with TermTree with DefTree with PatTree

  /*------------------------------- modifiers -------------------------------------*/

  type Mods >: Null <: Modifiers
  def emptyMods: Mods

  /*------------------------------- constructors -------------------------------------*/

  ///////////////////////// type trees
  def TypeIdent(name: String)(implicit unsafe: Unsafe): TypeTree

  def TypeSelect(qual: Tree, name: String): TypeTree

  def TypeSingleton(ref: Tree): TypeTree

  def TypeApply(tpe: TypeTree, args: List[TypeTree]): TypeTree

  def TypeInfix(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree

  def TypeFunction(params: List[TypeTree], res: TypeTree): TypeTree

  def TypeTuple(args: List[TypeTree]): TypeTree

  def TypeAnd(lhs: TypeTree, rhs: TypeTree): TypeTree

  def TypeOr(lhs: TypeTree, rhs: TypeTree): TypeTree

  def TypeRefine(stats: List[Tree]): TypeTree
  def TypeRefine(tpe: TypeTree, stats: List[Tree]): TypeTree

  def TypeBounds(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree

  def TypeRepeated(tpe: TypeTree): TypeTree

  def TypeByName(tpe: TypeTree): TypeTree

  def TypeAnnotated(tpe: TypeTree, annots: List[Tree]): TypeTree

  ///////////////////////// terms
  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  def Infix(lhs: TermTree, op: String, rhs: TermTree): TermTree

  def Prefix(op: String, od: TermTree): TermTree

  def Postfix(od: TermTree, op: String): TermTree

  def Throw(expr: TermTree): TermTree

  def Annotated(expr: TermTree, annots: List[Tree]): TermTree

  def If(cond: TermTree, thenp: TermTree, elsep: Option[TermTree]): TermTree

  def Try(expr: Tree, cases: List[Tree], finallyp: Option[TermTree]): TermTree
  def Try(expr: Tree, handler: Tree, finallyp: Option[TermTree]): TermTree

  // definition trees
  def NewAnonymClass(parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): TermTree

  def NewInstance(typeTree: TypeTree, argss: List[List[TermTree]]): TermTree

  def Function(params: List[Param], body: TermTree): TermTree

  def While(cond: TermTree, body: TermTree): TermTree

  def DoWhile(body: TermTree, cond: TermTree): TermTree

  // for comprehension
  def For: ForImpl
  trait ForImpl {
    def ForDo(enums: List[Tree], body: TermTree): TermTree
    def ForYield(enums: List[Tree], body: TermTree): TermTree
    def GenFrom(pat: PatTree, rhs: TermTree): Tree
    def GenAlias(pat: PatTree, rhs: TermTree): Tree
    def Guard(cond: TermTree): Tree
  }

  // named arguments
  def Named(name: String, expr: Tree): TermTree

  def Repeated(expr: Tree): TermTree

  def Apply(fun: TermTree, args: List[TermTree]): TermTree

  def ApplyType(fun: TermTree, args: List[TypeTree]): TermTree

  def Ident(name: String)(implicit unsafe: Unsafe): TermTree

  def Lit(value: Any): TermTree

  def This(qual: String): TermTree

  def Super(thisp: String, superp: String): TermTree

  def Select(qual: TermTree, name: String): TermTree

  def Ascribe(expr: TermTree, tpe: TypeTree): TermTree

  def Assign(lhs: TermTree, rhs: TermTree): TermTree

  def Update(fun: TermTree, argss: List[List[TermTree]], rhs: TermTree): TermTree

  def Return(expr: TermTree): TermTree
  def Return(): TermTree

  def Block(stats: List[Tree]): TermTree

  def PartialFunction(cases: List[Tree]): TermTree

  def Match(expr: TermTree, cases: List[Tree]): TermTree

  def Case(pat: PatTree, cond: Option[TermTree], body: TermTree): Tree

  def Tuple(args: List[TermTree]): TermTree

  def Interpolate(prefix: String, parts: List[String], args: List[TermTree]): TermTree

  ///////////////////////// def trees
  def TypeDecl(mods: Mods, name: String, tparams: List[TypeParam], tbounds: Option[TypeTree]): DefTree

  def TypeAlias(mods: Mods, name: String, tparams: List[TypeParam], rhs: TypeTree): DefTree

  def Param(mods: Mods, name: String, tpe: Option[TypeTree], default: Option[TermTree]): Param

  def TypeParam(mods: Mods, name: String, tparams: List[TypeParam], tbounds: Option[TypeTree], cbounds: List[TypeTree]): TypeParam

  def PatDef(mods: Mods, lhs: PatTree, tpe: Option[TypeTree], rhs: Tree): DefTree

  // extends qual.T[A, B](x, y)(z)
  def InitCall(tpe: TypeTree, argss: List[List[TermTree]]): InitCall

  def Self(name: String, tpe: TypeTree): Self
  def Self(name: String): Self

  def ValDef(mods: Mods, name: String, tpe: Option[TypeTree], rhs: Tree): ValDef

  def ValDecl(mods: Mods, name: String, tpe: TypeTree): ValDecl

  def DefDef(mods: Mods, name: String, tparams: List[TypeParam], paramss: List[List[Param]], tpe: Option[TypeTree], rhs: Tree): DefDef

  def DefDecl(mods: Mods, name: String, tparams: List[TypeParam], paramss: List[List[Param]], tpe: TypeTree): DefDecl

  def Class(mods: Mods, name: String, tparams: List[TypeParam], ctorMods: Mods, paramss: List[List[Param]], parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Class

  def Trait(mods: Mods, name: String, tparams: List[TypeParam], ctorMods: Mods, paramss: List[List[Param]], parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Trait

  def Object(mods: Mods, name: String, parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Object

  // patterns
  def Pat: PatImpl
  trait PatImpl {
    def Var(name: String): PatTree
    def Ascribe(name: String, tp: TypeTree): PatTree
    def Bind(name: String, expr: PatTree): PatTree
    def Ident(name: String): PatTree
    def Lit(value: Any): PatTree
    def Alt(trees: List[PatTree]): PatTree
    def Unapply(fun: TermTree, args: List[PatTree]): PatTree
    def Infix(lhs: PatTree, op: String, rhs: PatTree): PatTree
    def Tuple(pats: List[PatTree]): PatTree
  }

  // importees
  def Import: ImportImpl
  trait ImportImpl {
    def apply(items: List[Tree]): Tree
    def Item(prefix: Tree, importees: List[Tree]): Tree
    def Name(name: String): Tree
    def Rename(from: String, to: String): Tree
    def Hide(name: String): Tree
  }

  /*------------------------------- TreeOps-------------------------------------*/
  def pos(tree: Tree): Position

  def show(tree: Tree): String
}
