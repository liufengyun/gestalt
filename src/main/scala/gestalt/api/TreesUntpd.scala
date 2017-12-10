package scala.gestalt.api

import scala.gestalt.{ toolbox => impl, _ }

object Untpd {

  type Tree     >: Null <: AnyRef
  type TypeTree >: Null <: Tree
  type TermTree >: Null <: Tree
  type DefTree  >: Null <: Tree
  type PatTree  >: Null <: Tree

  type Splice   <: TypeTree with TermTree with DefTree with PatTree

  type Ident    <: TermTree

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

  /*----------------------------------- modifiers ------------------------------------*/

  type Mods = core.Modifiers
  def emptyMods: Mods = !impl.untpd.emptyMods

  /*------------------------------- constructors -------------------------------------*/

  def root: TermTree = Ident("_root_")
  def empty: TermTree = Ident("_empty_")

  def TypedSplice(tree: Tpd.Tree): Splice =
    !impl.untpd.TypedSplice(!tree)

  ////////////////////////////// terms
  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  def Infix(lhs: TermTree, op: String, rhs: TermTree): TermTree =
    !impl.untpd.Term.Infix(!lhs, op, !rhs)

  def Prefix(op: String, od: TermTree): TermTree =
    !impl.untpd.Term.Prefix(op, !od)

  def Postfix(od: TermTree, op: String): TermTree =
    !impl.untpd.Term.Postfix(!od, op)

  def Throw(expr: TermTree): TermTree =
    !impl.untpd.Term.Throw(!expr)

  def Annotated(expr: TermTree, annots: List[Tree]): TermTree =
    !impl.untpd.Term.Annotated(!expr, !annots)

  def If(cond: TermTree, thenp: TermTree, elsep: Option[TermTree]): TermTree =
    !impl.untpd.Term.If(!cond, !thenp, !elsep)

  def If(cond: TermTree, thenp: TermTree, elsep: TermTree): TermTree =
    If(cond, thenp, Some(elsep))

  def Try(expr: Tree, cases: List[Tree], finallyp: Option[TermTree]): TermTree = {
    val untpd = impl.untpd
    !untpd.Term.Try(!expr, cases.asInstanceOf[List[untpd.Tree]], !finallyp)
  }

  def Try(expr: Tree, handler: Tree, finallyp: Option[TermTree]): TermTree = {
    val untpd = impl.untpd
    !untpd.Term.Try(
      expr.asInstanceOf[untpd.Tree],
      handler.asInstanceOf[untpd.Tree],
      finallyp.asInstanceOf[Option[untpd.TermTree]]
    )
  }

  // definition trees
  def NewAnonymClass(parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): TermTree =
    !impl.untpd.Term.NewAnonymClass(!parents, !selfOpt, !stats)

  def NewInstance(typeTree: TypeTree, argss: List[List[TermTree]]): TermTree =
    !impl.untpd.Term.NewInstance(!typeTree, !argss)

  def Function(params: List[Param], body: TermTree): TermTree =
    !impl.untpd.Term.Function(!params, !body)

  def While(cond: TermTree, body: TermTree): TermTree =
    !impl.untpd.Term.While(!cond, !body)

  def DoWhile(body: TermTree, cond: TermTree): TermTree =
    !impl.untpd.Term.DoWhile(!body, !cond)

  // for comprehension
  object For {
    def ForDo(enums: List[Tree], body: TermTree): TermTree =
      !impl.untpd.Term.For.ForDo(!enums, !body)

    def ForYield(enums: List[Tree], body: TermTree): TermTree =
      !impl.untpd.Term.For.ForYield(!enums, !body)

    def GenFrom(pat: PatTree, rhs: TermTree): Tree =
      !impl.untpd.Term.For.GenFrom(!pat, !rhs)

    def GenAlias(pat: PatTree, rhs: TermTree): Tree =
      !impl.untpd.Term.For.GenAlias(!pat, !rhs)

    def Guard(cond: TermTree): Tree =
      !impl.untpd.Term.For.Guard(!cond)
  }

  // named arguments
  def Named(name: String, expr: Tree): TermTree =
    !impl.untpd.Term.Named(name, !expr)

  def Repeated(expr: Tree): TermTree =
    !impl.untpd.Term.Repeated(!expr)

  def Apply(fun: TermTree, args: List[TermTree]): TermTree =
    !impl.untpd.Term.Apply(!fun, !args)

  def ApplyType(fun: TermTree, args: List[TypeTree]): TermTree =
    !impl.untpd.Term.ApplyType(!fun, !args)

  def Ident(name: String)(implicit unsafe: Unsafe): Ident =
    !impl.untpd.Term.Ident(name)(!unsafe)

  def Lit(value: Any): TermTree =
    !impl.untpd.Term.Lit(value)

  def Ident(name: "_root_"): TermTree =
    !impl.untpd.Term.Ident(name)(!options.unsafe)

  def Ident(name: "scala")(implicit dummy: core.Dummy): TermTree =
    !impl.untpd.Term.Ident(name)(!options.unsafe)

  def Ident(name: "java")(implicit dummy: core.Dummy1): TermTree =
    !impl.untpd.Term.Ident(name)(!options.unsafe)

  def Ident(name: "_empty_")(implicit dummy: core.Dummy2): TermTree =
    !impl.untpd.Term.Ident("<empty>")(!options.unsafe)

  def This(qual: String): TermTree =
    !impl.untpd.Term.This(qual)

  def Super(thisp: String, superp: String): TermTree =
    !impl.untpd.Term.Super(thisp, superp)

  def Select(qual: TermTree, name: String): TermTree =
    !impl.untpd.Term.Select(!qual, name)

  def Ascribe(expr: TermTree, tpe: TypeTree): TermTree =
    !impl.untpd.Term.Ascribe(!expr, !tpe)

  def Assign(lhs: TermTree, rhs: TermTree): TermTree =
    !impl.untpd.Term.Assign(!lhs, !rhs)

  def Update(fun: TermTree, argss: List[List[TermTree]], rhs: TermTree): TermTree =
    !impl.untpd.Term.Update(!fun, !argss, !rhs)

  def Return(expr: TermTree): TermTree =
    !impl.untpd.Term.Return(!expr)

  def Return(): TermTree =
    !impl.untpd.Term.Return()

  def Block(stats: List[Tree]): TermTree =
    !impl.untpd.Term.Block(!stats)

  def PartialFunction(cases: List[Tree]): TermTree =
    !impl.untpd.Term.PartialFunction(!cases)

  def Match(expr: TermTree, cases: List[Tree]): TermTree =
    !impl.untpd.Term.Match(!expr, !cases)

  def Case(pat: PatTree, cond: Option[TermTree], body: TermTree): Tree =
    !impl.untpd.Term.Case(!pat, !cond, !body)

  def Tuple(args: List[TermTree]): TermTree =
    !impl.untpd.Term.Tuple(!args)

  def Interpolate(prefix: String, parts: List[String], args: List[TermTree]): TermTree =
    !impl.untpd.Term.Interpolate(prefix, parts, !args)

  // extensions
  def ApplySeq(fun: TermTree, argss: List[List[TermTree]]): TermTree =
    argss.foldLeft(fun) { (acc, args) => !impl.untpd.Term.Apply(!acc, !args) }

  ////////////////////////////// type trees
  def TypeIdent(name: String)(implicit unsafe: Unsafe): TypeTree =
    !impl.untpd.Type.Ident(name)(!unsafe)

  def TypeSelect(qual: Tree, name: String): TypeTree =
    !impl.untpd.Type.Select(!qual, name)

  def TypeSingleton(ref: Tree): TypeTree =
    !impl.untpd.Type.Singleton(!ref)

  def TypeApply(tpe: TypeTree, args: List[TypeTree]): TypeTree =
    !impl.untpd.Type.Apply(!tpe, !args)

  def TypeInfix(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree =
    !impl.untpd.Type.Infix(!lhs, op, !rhs)

  def TypeFunction(params: List[TypeTree], res: TypeTree): TypeTree =
    !impl.untpd.Type.Function(!params, !res)

  def TypeTuple(args: List[TypeTree]): TypeTree =
    !impl.untpd.Type.Tuple(!args)

  def TypeAnd(lhs: TypeTree, rhs: TypeTree): TypeTree =
    !impl.untpd.Type.And(!lhs, !rhs)

  def TypeOr(lhs: TypeTree, rhs: TypeTree): TypeTree =
    !impl.untpd.Type.Or(!lhs, !rhs)

  def TypeRefine(stats: List[Tree]): TypeTree =
    !impl.untpd.Type.Refine(!stats)

  def TypeRefine(tpe: TypeTree, stats: List[Tree]): TypeTree =
    !impl.untpd.Type.Refine(!tpe, !stats)

  def TypeBounds(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree =
    !impl.untpd.Type.Bounds(!lo, !hi)

  def TypeRepeated(tpe: TypeTree): TypeTree =
    !impl.untpd.Type.Repeated(!tpe)

  def TypeByName(tpe: TypeTree): TypeTree =
    !impl.untpd.Type.ByName(!tpe)

  def TypeAnnotated(tpe: TypeTree, annots: List[Tree]): TypeTree =
    !impl.untpd.Type.Annotated(!tpe, !annots)


  ////////////////////////////// def trees
  def TypeDecl(mods: Mods, name: String, tparams: List[TypeParam], tbounds: Option[TypeTree]): DefTree =
    !impl.untpd.Defn.TypeDecl(!mods, name, !tparams, !tbounds)

  def TypeAlias(mods: Mods, name: String, tparams: List[TypeParam], rhs: TypeTree): DefTree =
    !impl.untpd.Defn.TypeAlias(!mods, name, !tparams, !rhs)

  def Param(mods: Mods, name: String, tpe: Option[TypeTree], default: Option[TermTree]): Param =
    !impl.untpd.Defn.Param(!mods, name, !tpe, !default)

  def Param(name: String): Param = Param(emptyMods, name, None, None)
  def Param(name: String, tpe: TypeTree): Param = Param(emptyMods, name, Some(tpe), None)

  def TypeParam(mods: Mods, name: String, tparams: List[TypeParam], tbounds: Option[TypeTree], cbounds: List[TypeTree]): TypeParam =
    !impl.untpd.Defn.TypeParam(!mods, name, !tparams, !tbounds, !cbounds)

  def TypeParam(name: String, tbounds: TypeTree): TypeParam = TypeParam(emptyMods, name, Nil, Some(tbounds), Nil)

  // extends qual.T[A, B](x, y)(z)
  def InitCall(tpe: TypeTree, argss: List[List[TermTree]]): InitCall =
    !impl.untpd.Defn.InitCall(!tpe, !argss)

  def Self(name: String, tpe: TypeTree): Self =
    !impl.untpd.Defn.Self(name, !tpe)

  def Self(name: String): Self =
    !impl.untpd.Defn.Self(name)

  def ValDef(mods: Mods, name: String, tpe: Option[TypeTree], rhs: Tree): ValDef =
    !impl.untpd.Defn.ValDef(!mods, name, !tpe, !rhs)

  def ValDecl(mods: Mods, name: String, tpe: TypeTree): ValDecl =
    !impl.untpd.Defn.ValDecl(!mods, name, !tpe)

  def PatDef(mods: Mods, lhs: PatTree, tpe: Option[TypeTree], rhs: Tree): DefTree =
    !impl.untpd.Defn.PatDef(!mods, !lhs, !tpe, !rhs)

  def DefDef(mods: Mods, name: String, tparams: List[TypeParam], paramss: List[List[Param]], tpe: Option[TypeTree], rhs: Tree): DefDef =
    !impl.untpd.Defn.DefDef(!mods, name, !tparams, !paramss, !tpe, !rhs)

  def DefDecl(mods: Mods, name: String, tparams: List[TypeParam], paramss: List[List[Param]], tpe: TypeTree): DefDecl =
    !impl.untpd.Defn.DefDecl(!mods, name, !tparams, !paramss, !tpe)

  def Class(mods: Mods, name: String, tparams: List[TypeParam], ctorMods: Mods, paramss: List[List[Param]], parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Class =
    !impl.untpd.Defn.Class(!mods, name, !tparams, !ctorMods, !paramss, !parents, !selfOpt, !stats)

  def Trait(mods: Mods, name: String, tparams: List[TypeParam], ctorMods: Mods, paramss: List[List[Param]], parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Trait =
    !impl.untpd.Defn.Trait(!mods, name, !tparams, !ctorMods, !paramss, !parents, !selfOpt, !stats)

  def Object(mods: Mods, name: String, parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Object =
    !impl.untpd.Defn.Object(!mods, name, !parents, !selfOpt, !stats)


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
