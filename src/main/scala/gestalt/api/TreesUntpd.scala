package scala.gestalt.api

import scala.gestalt.{ toolbox => impl, _ }

object Untpd {

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

  type Splice   <: TypeTree with TermTree with DefTree with PatTree with InitCall

  /*----------------------------------- modifiers ------------------------------------*/

  type Mods = core.Modifiers
  def emptyMods: Mods = !impl.untpd.emptyMods

  /*------------------------------- constructors -------------------------------------*/

  def root: TermTree = Ident("_root_")
  def empty: TermTree = Ident("_empty_")

  ////////////////////////////// terms
  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  def Infix(lhs: TermTree, op: String, rhs: TermTree): TermTree =
    !impl.untpd.Infix(!lhs, op, !rhs)

  def Prefix(op: String, od: TermTree): TermTree =
    !impl.untpd.Prefix(op, !od)

  def Postfix(od: TermTree, op: String): TermTree =
    !impl.untpd.Postfix(!od, op)

  def Throw(expr: TermTree): TermTree =
    !impl.untpd.Throw(!expr)

  def Annotated(expr: TermTree, annots: List[Tree]): TermTree =
    !impl.untpd.Annotated(!expr, !annots)

  def If(cond: TermTree, thenp: TermTree, elsep: Option[TermTree]): TermTree =
    !impl.untpd.If(!cond, !thenp, !elsep)

  def If(cond: TermTree, thenp: TermTree, elsep: TermTree): TermTree =
    If(cond, thenp, Some(elsep))

  def Try(expr: Tree, cases: List[Tree], finallyp: Option[TermTree]): TermTree = {
    val untpd = impl.untpd
    !untpd.Try(!expr, cases.asInstanceOf[List[untpd.Tree]], !finallyp)
  }

  def Try(expr: Tree, handler: Tree, finallyp: Option[TermTree]): TermTree = {
    val untpd = impl.untpd
    !untpd.Try(
      expr.asInstanceOf[untpd.Tree],
      handler.asInstanceOf[untpd.Tree],
      finallyp.asInstanceOf[Option[untpd.TermTree]]
    )
  }

  // definition trees
  def NewAnonymClass(parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): TermTree =
    !impl.untpd.NewAnonymClass(!parents, !selfOpt, !stats)

  def NewInstance(typeTree: TypeTree, argss: List[List[TermTree]]): TermTree =
    !impl.untpd.NewInstance(!typeTree, !argss)

  def Function(params: List[Param], body: TermTree): TermTree =
    !impl.untpd.Function(!params, !body)

  def While(cond: TermTree, body: TermTree): TermTree =
    !impl.untpd.While(!cond, !body)

  def DoWhile(body: TermTree, cond: TermTree): TermTree =
    !impl.untpd.DoWhile(!body, !cond)

  // for comprehension
  object For {
    def ForDo(enums: List[Tree], body: TermTree): TermTree =
      !impl.untpd.For.ForDo(!enums, !body)

    def ForYield(enums: List[Tree], body: TermTree): TermTree =
      !impl.untpd.For.ForYield(!enums, !body)

    def GenFrom(pat: PatTree, rhs: TermTree): Tree =
      !impl.untpd.For.GenFrom(!pat, !rhs)

    def GenAlias(pat: PatTree, rhs: TermTree): Tree =
      !impl.untpd.For.GenAlias(!pat, !rhs)

    def Guard(cond: TermTree): Tree =
      !impl.untpd.For.Guard(!cond)
  }

  // named arguments
  def Named(name: String, expr: Tree): TermTree =
    !impl.untpd.Named(name, !expr)

  def Repeated(expr: Tree): TermTree =
    !impl.untpd.Repeated(!expr)

  def Apply(fun: TermTree, args: List[TermTree]): TermTree =
    !impl.untpd.Apply(!fun, !args)

  def ApplyType(fun: TermTree, args: List[TypeTree]): TermTree =
    !impl.untpd.ApplyType(!fun, !args)

  def Ident(name: String)(implicit unsafe: Unsafe): TermTree =
    !impl.untpd.Ident(name)(!unsafe)

  def Lit(value: Any): TermTree =
    !impl.untpd.Lit(value)

  def Ident(name: "_root_"): TermTree =
    !impl.untpd.Ident(name)(!options.unsafe)

  def Ident(name: "scala")(implicit dummy: core.Dummy): TermTree =
    !impl.untpd.Ident(name)(!options.unsafe)

  def Ident(name: "java")(implicit dummy: core.Dummy1): TermTree =
    !impl.untpd.Ident(name)(!options.unsafe)

  def Ident(name: "_empty_")(implicit dummy: core.Dummy2): TermTree =
    !impl.untpd.Ident("<empty>")(!options.unsafe)

  def This(qual: String): TermTree =
    !impl.untpd.This(qual)

  def Super(thisp: String, superp: String): TermTree =
    !impl.untpd.Super(thisp, superp)

  def Select(qual: TermTree, name: String): TermTree =
    !impl.untpd.Select(!qual, name)

  def Ascribe(expr: TermTree, tpe: TypeTree): TermTree =
    !impl.untpd.Ascribe(!expr, !tpe)

  def Assign(lhs: TermTree, rhs: TermTree): TermTree =
    !impl.untpd.Assign(!lhs, !rhs)

  def Update(fun: TermTree, argss: List[List[TermTree]], rhs: TermTree): TermTree =
    !impl.untpd.Update(!fun, !argss, !rhs)

  def Return(expr: TermTree): TermTree =
    !impl.untpd.Return(!expr)

  def Return(): TermTree =
    !impl.untpd.Return()

  def Block(stats: List[Tree]): TermTree =
    !impl.untpd.Block(!stats)

  def PartialFunction(cases: List[Tree]): TermTree =
    !impl.untpd.PartialFunction(!cases)

  def Match(expr: TermTree, cases: List[Tree]): TermTree =
    !impl.untpd.Match(!expr, !cases)

  def Case(pat: PatTree, cond: Option[TermTree], body: TermTree): Tree =
    !impl.untpd.Case(!pat, !cond, !body)

  def Tuple(args: List[TermTree]): TermTree =
    !impl.untpd.Tuple(!args)

  def Interpolate(prefix: String, parts: List[String], args: List[TermTree]): TermTree =
    !impl.untpd.Interpolate(prefix, parts, !args)

  // extensions
  def ApplySeq(fun: TermTree, argss: List[List[TermTree]]): TermTree =
    argss.foldLeft(fun) { (acc, args) => !impl.untpd.Apply(!acc, !args) }

  ////////////////////////////// type trees
  def TypeIdent(name: String)(implicit unsafe: Unsafe): TypeTree =
    !impl.untpd.TypeIdent(name)(!unsafe)

  def TypeSelect(qual: Tree, name: String): TypeTree =
    !impl.untpd.TypeSelect(!qual, name)

  def TypeSingleton(ref: Tree): TypeTree =
    !impl.untpd.TypeSingleton(!ref)

  def TypeApply(tpe: TypeTree, args: List[TypeTree]): TypeTree =
    !impl.untpd.TypeApply(!tpe, !args)

  def TypeInfix(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree =
    !impl.untpd.TypeInfix(!lhs, op, !rhs)

  def TypeFunction(params: List[TypeTree], res: TypeTree): TypeTree =
    !impl.untpd.TypeFunction(!params, !res)

  def TypeTuple(args: List[TypeTree]): TypeTree =
    !impl.untpd.TypeTuple(!args)

  def TypeAnd(lhs: TypeTree, rhs: TypeTree): TypeTree =
    !impl.untpd.TypeAnd(!lhs, !rhs)

  def TypeOr(lhs: TypeTree, rhs: TypeTree): TypeTree =
    !impl.untpd.TypeOr(!lhs, !rhs)

  def TypeRefine(stats: List[Tree]): TypeTree =
    !impl.untpd.TypeRefine(!stats)

  def TypeRefine(tpe: TypeTree, stats: List[Tree]): TypeTree =
    !impl.untpd.TypeRefine(!tpe, !stats)

  def TypeBounds(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree =
    !impl.untpd.TypeBounds(!lo, !hi)

  def TypeRepeated(tpe: TypeTree): TypeTree =
    !impl.untpd.TypeRepeated(!tpe)

  def TypeByName(tpe: TypeTree): TypeTree =
    !impl.untpd.TypeByName(!tpe)

  def TypeAnnotated(tpe: TypeTree, annots: List[Tree]): TypeTree =
    !impl.untpd.TypeAnnotated(!tpe, !annots)


  ////////////////////////////// def trees
  def TypeDecl(mods: Mods, name: String, tparams: List[TypeParam], tbounds: Option[TypeTree]): DefTree =
    !impl.untpd.TypeDecl(!mods, name, !tparams, !tbounds)

  def TypeAlias(mods: Mods, name: String, tparams: List[TypeParam], rhs: TypeTree): DefTree =
    !impl.untpd.TypeAlias(!mods, name, !tparams, !rhs)

  def Param(mods: Mods, name: String, tpe: Option[TypeTree], default: Option[TermTree]): Param =
    !impl.untpd.Param(!mods, name, !tpe, !default)

  def Param(name: String): Param = Param(emptyMods, name, None, None)
  def Param(name: String, tpe: TypeTree): Param = Param(emptyMods, name, Some(tpe), None)

  def TypeParam(mods: Mods, name: String, tparams: List[TypeParam], tbounds: Option[TypeTree], cbounds: List[TypeTree]): TypeParam =
    !impl.untpd.TypeParam(!mods, name, !tparams, !tbounds, !cbounds)

  def TypeParam(name: String, tbounds: TypeTree): TypeParam = TypeParam(emptyMods, name, Nil, Some(tbounds), Nil)

  // extends qual.T[A, B](x, y)(z)
  def InitCall(tpe: TypeTree, argss: List[List[TermTree]]): InitCall =
    !impl.untpd.InitCall(!tpe, !argss)

  def Self(name: String, tpe: TypeTree): Self =
    !impl.untpd.Self(name, !tpe)

  def Self(name: String): Self =
    !impl.untpd.Self(name)

  def ValDef(mods: Mods, name: String, tpe: Option[TypeTree], rhs: Tree): ValDef =
    !impl.untpd.ValDef(!mods, name, !tpe, !rhs)

  def ValDecl(mods: Mods, name: String, tpe: TypeTree): ValDecl =
    !impl.untpd.ValDecl(!mods, name, !tpe)

  def PatDef(mods: Mods, lhs: PatTree, tpe: Option[TypeTree], rhs: Tree): DefTree =
    !impl.untpd.PatDef(!mods, !lhs, !tpe, !rhs)

  def DefDef(mods: Mods, name: String, tparams: List[TypeParam], paramss: List[List[Param]], tpe: Option[TypeTree], rhs: Tree): DefDef =
    !impl.untpd.DefDef(!mods, name, !tparams, !paramss, !tpe, !rhs)

  def DefDecl(mods: Mods, name: String, tparams: List[TypeParam], paramss: List[List[Param]], tpe: TypeTree): DefDecl =
    !impl.untpd.DefDecl(!mods, name, !tparams, !paramss, !tpe)

  def Class(mods: Mods, name: String, tparams: List[TypeParam], ctorMods: Mods, paramss: List[List[Param]], parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Class =
    !impl.untpd.Class(!mods, name, !tparams, !ctorMods, !paramss, !parents, !selfOpt, !stats)

  def Trait(mods: Mods, name: String, tparams: List[TypeParam], ctorMods: Mods, paramss: List[List[Param]], parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Trait =
    !impl.untpd.Trait(!mods, name, !tparams, !ctorMods, !paramss, !parents, !selfOpt, !stats)

  def Object(mods: Mods, name: String, parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Object =
    !impl.untpd.Object(!mods, name, !parents, !selfOpt, !stats)


  // patterns
  object Pat {
    // variable pattern
    def Var(name: String): PatTree =
      !impl.untpd.Pat.Var(name)

    def Ascribe(name: String, tp: TypeTree): PatTree =
      !impl.untpd.Pat.Ascribe(name, !tp)

    def Bind(name: String, expr: PatTree): PatTree =
      !impl.untpd.Pat.Bind(name, !expr)

    def Ident(name: String): PatTree =
      !impl.untpd.Pat.Ident(name)

    def Lit(value: Any): PatTree =
      !impl.untpd.Pat.Lit(value)

    def Alt(trees: List[PatTree]): PatTree =
      !impl.untpd.Pat.Alt(!trees)

    def Unapply(fun: TermTree, args: List[PatTree]): PatTree =
      !impl.untpd.Pat.Unapply(!fun, !args)

    def Infix(lhs: PatTree, op: String, rhs: PatTree): PatTree =
      !impl.untpd.Pat.Infix(!lhs, op, !rhs)

    def Tuple(pats: List[PatTree]): PatTree =
      !impl.untpd.Pat.Tuple(!pats)
  }

  // importees
  object Import {
    def apply(items: List[Tree]): Tree =
      !impl.untpd.Import.apply(!items)

    def Item(prefix: Tree, importees: List[Tree]): Tree =
      !impl.untpd.Import.Item(!prefix, !importees)


    def Name(name: String): Tree =
      !impl.untpd.Import.Name(name)

    def Rename(from: String, to: String): Tree =
      !impl.untpd.Import.Rename(from, to)

    def Hide(name: String): Tree =
      !impl.untpd.Import.Hide(name)
  }

  /*------------------------------- TreeOps-------------------------------------*/
  def pos(tree: Tree): Position = !impl.tpd.pos(!tree)

  def show(tree: Tree): String = impl.untpd.show(!tree)
}
