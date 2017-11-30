package scala.gestalt.core

trait Positions { this: Toolbox =>
  // it's safe to assume type trees and untyped trees use the same modelling of position
  type Pos

  def Pos: PosImpl
  trait PosImpl {
    def pos(tree: Tree): Pos
    def pos(tree: tpd.Tree)(implicit c: Dummy): Pos
  }
}

trait Trees extends Positions { toolbox: Toolbox =>

 // safety by construction -- implementation can have TypeTree = Tree
  type Tree     >: Null <: AnyRef
  type TypeTree >: Null <: Tree
  type TermTree >: Null <: Tree
  type DefTree  >: Null <: Tree
  type PatTree  >: Null <: Tree

  type Splice   <: TypeTree with TermTree with DefTree with PatTree
  type Ident    <: TermTree with PatTree

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

  type Context

  type Mods >: Null <: Modifiers

  trait Modifiers {
    def isPrivate: Boolean
    def isProtected: Boolean
    def isOverride: Boolean
    def isFinal: Boolean
    def isImplicit: Boolean
    def isLazy: Boolean
    def isSealed: Boolean
    def isAbstract: Boolean
    def isValParam: Boolean
    def isVarParam: Boolean
    def isCase: Boolean
    def isContravariant: Boolean
    def isCovariant: Boolean
    def isInline: Boolean
    def isMutable: Boolean
    def privateWithin: String
    def hasAnnotations: Boolean

    // can be empty or `this`
    def setPrivate(within: String): Mods
    def setProtected(within: String): Mods
    def setOverride: Mods
    def setFinal: Mods
    def setImplicit: Mods
    def setLazy: Mods
    def setSealed: Mods
    def setAbstract: Mods
    def setValParam: Mods
    def setVarParam: Mods
    def setCase: Mods
    def setContravariant: Mods
    def setCovariant: Mods
    def setInline: Mods
    def setMutable: Mods

    def withAddedAnnotation(annot: Tree): Mods
    def withAnnotations(annots: List[Tree]): Mods
    def annotations: List[Tree]
  }

  // modifiers
  def emptyMods: Mods

  // definition trees
  def NewAnonymClass: NewAnonymClassImpl
  trait NewAnonymClassImpl {
    def apply(parents: List[InitCall], self: Option[Self], stats: List[Tree]): DefTree
    def apply(parents: List[Type], stats: List[tpd.Tree]): tpd.Tree
  }

  def TypeDecl: TypeDeclImpl
  trait TypeDeclImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], tbounds: Option[TypeTree]): DefTree
  }

  def TypeAlias: TypeAliasImpl
  trait TypeAliasImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], rhs: TypeTree): DefTree
  }

  def PatDef: PatDefImpl
  trait PatDefImpl {
    def apply(mods: Mods, lhs: PatTree, tpe: Option[TypeTree], rhs: Tree): DefTree
  }

  // extends qual.T[A, B](x, y)(z)
  def InitCall: InitCallImpl
  trait InitCallImpl {
    def apply(tpe: TypeTree, argss: List[List[TermTree]]): InitCall
  }

  def NewInstance: NewInstanceImpl
  trait NewInstanceImpl {
    def apply(typeTree: TypeTree, argss: List[List[TermTree]]): TermTree

    def apply(tp: Type, argss: List[List[tpd.Tree]])(implicit cap: Dummy): tpd.Tree
  }

  def SecondaryCtor: SecondaryCtorImpl
  trait SecondaryCtorImpl {
    def apply(mods: Mods, paramss: List[List[Param]], rhs: TermTree): Tree
  }

  def Self: SelfImpl
  trait SelfImpl {
    def apply(name: String, tpe: TypeTree): Self
    def apply(name: String): Self
  }

  // type trees
  def TypeIdent: TypeIdentImpl
  trait TypeIdentImpl {
    def apply(name: String)(implicit unsafe: Unsafe): TypeTree
  }

  def TypeSelect: TypeSelectImpl
  trait TypeSelectImpl {
    def apply(qual: Tree, name: String): TypeTree
  }

  def TypeSingleton: TypeSingletonImpl
  trait TypeSingletonImpl {
    def apply(ref: Tree): TypeTree
  }

  def TypeApply: TypeApplyImpl
  trait TypeApplyImpl {
    def apply(tpe: TypeTree, args: List[TypeTree]): TypeTree
  }

  def TypeInfix: TypeInfixImpl
  trait TypeInfixImpl {
    def apply(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree
  }

  def TypeFunction: TypeFunctionImpl
  trait TypeFunctionImpl {
    def apply(params: List[TypeTree], res: TypeTree): TypeTree
  }

  def TypeTuple: TypeTupleImpl
  trait TypeTupleImpl {
    def apply(args: List[TypeTree]): TypeTree
  }

  def TypeAnd: TypeAndImpl
  trait TypeAndImpl {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree
  }

  def TypeOr: TypeOrImpl
  trait TypeOrImpl {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree
  }

  def TypeRefine: TypeRefineImpl
  trait TypeRefineImpl {
    def apply(stats: List[Tree]): TypeTree
    def apply(tpe: TypeTree, stats: List[Tree]): TypeTree
  }

  def TypeBounds: TypeBoundsImpl
  trait TypeBoundsImpl {
    def apply(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree
  }

  def TypeRepeated: TypeRepeatedImpl
  trait TypeRepeatedImpl {
    def apply(tpe: TypeTree): TypeTree
  }

  def TypeByName: TypeByNameImpl
  trait TypeByNameImpl {
    def apply(tpe: TypeTree): TypeTree
  }

  def TypeAnnotated: TypeAnnotatedImpl
  trait TypeAnnotatedImpl {
    def apply(tpe: TypeTree, annots: List[Tree]): TypeTree
  }

  // terms
  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  def Infix: InfixImpl
  trait InfixImpl {
    def apply(lhs: TermTree, op: String, rhs: TermTree): TermTree
  }

  def Prefix: PrefixImpl
  trait PrefixImpl {
    def apply(op: String, od: TermTree): TermTree
  }

  def Postfix: PostfixImpl
  trait PostfixImpl {
    def apply(od: TermTree, op: String): TermTree
  }

  def Throw: ThrowImpl
  trait ThrowImpl {
    def apply(expr: TermTree): TermTree
  }

  def Annotated: AnnotatedImpl
  trait AnnotatedImpl {
    def apply(expr: TermTree, annots: List[Tree]): TermTree
  }

  def If: IfImpl
  trait IfImpl {
    def apply(cond: TermTree, thenp: TermTree, elsep: TermTree): TermTree =
      apply(cond, thenp, Some(elsep))

    def apply(cond: TermTree, thenp: TermTree, elsep: Option[TermTree]): TermTree

    def apply(cond: tpd.Tree, thenp: tpd.Tree, elsep: tpd.Tree)(implicit c: Dummy): tpd.Tree
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, tpd.Tree, tpd.Tree)]
  }

  def Try: TryImpl
  trait TryImpl {
    def apply(expr: Tree, cases: List[Tree], finallyp: Option[TermTree]): TermTree
    def apply(expr: TermTree, handler: Tree, finallyp: Option[TermTree]): TermTree
  }

  def Function: FunctionImpl
  trait FunctionImpl {
    def apply(params: List[Param], body: TermTree): TermTree
    def apply(params: List[Type], resTp: Type)(bodyFn: List[tpd.RefTree] => tpd.Tree): tpd.Tree
    def apply(params: List[Symbol], body: tpd.Tree)(implicit c: Dummy): tpd.Tree
    def unapply(tree: tpd.Tree): Option[(List[Symbol], tpd.Tree)]
  }

  def While: WhileImpl
  trait WhileImpl {
    def apply(cond: TermTree, body: TermTree): TermTree

    def apply(cond: tpd.Tree, body: tpd.Tree)(implicit c: Dummy): tpd.Tree
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, tpd.Tree)]
  }

  def DoWhile: DoWhileImpl
  trait DoWhileImpl {
    def apply(body: TermTree, cond: TermTree): TermTree

    def apply(body: tpd.Tree, cond: tpd.Tree)(implicit c: Dummy): tpd.Tree
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, tpd.Tree)]
  }

  def For: ForImpl
  trait ForImpl {
    def ForDo(enums: List[Tree], body: TermTree): TermTree
    def ForYield(enums: List[Tree], body: TermTree): TermTree
    def GenFrom(pat: PatTree, rhs: TermTree): Tree
    def GenAlias(pat: PatTree, rhs: TermTree): Tree
    def Guard(cond: TermTree): Tree
  }

  def Named: NamedImpl
  trait NamedImpl {
    def apply(name: String, expr: Tree): TermTree
  }

  def Repeated: RepeatedImpl
  trait RepeatedImpl {
    def apply(expr: Tree): TermTree
  }

  // patterns
  def Pat: PatImpl
  trait PatImpl {
    def Var(name: String): PatTree
    def Ascribe(name: String, tp: TypeTree): PatTree
    def Bind(name: String, expr: PatTree): PatTree
    def Ident(name: String)(implicit unsafe: Unsafe): PatTree = toolbox.Ident(name)
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

  // extractors
  def Lit: LitImpl
  trait LitImpl {
    def apply(value: Any): tpd.Tree
    def unapply(tree: tpd.Tree): Option[Any]
  }

  def Apply: ApplyImpl
  trait ApplyImpl {
    def apply(fun: TermTree, args: List[TermTree]): TermTree
    def apply(fun: tpd.Tree, args: List[tpd.Tree])(implicit c: Dummy): tpd.Tree
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, List[tpd.Tree])]
  }

  def ApplyType: ApplyTypeImpl
  trait ApplyTypeImpl {
    def apply(fun: TermTree, args: List[TypeTree]): TermTree

    def apply(fun: tpd.Tree, args: List[Type])(implicit c: Dummy): tpd.Tree
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, List[Type])]
  }

  def Ident: IdentImpl
  trait IdentImpl {
    def apply(name: String)(implicit unsafe: Unsafe): Ident
    def apply(symbol: Symbol): tpd.RefTree
    def unapply(tree: tpd.Tree): Option[Symbol]
  }

  def This: ThisImpl
  trait ThisImpl {
    def apply(qual: String): TermTree
  }

  def Super: SuperImpl
  trait SuperImpl {
    def apply(thisp: String, superp: String): TermTree
  }

  def Select: SelectImpl
  trait SelectImpl {
    def apply(qual: TermTree, name: String): TermTree

    def apply(qual: tpd.Tree, name: String)(implicit c: Dummy): tpd.RefTree
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, Symbol)]
  }

  def Ascribe: AscribeImpl
  trait AscribeImpl {
    def apply(expr: TermTree, tpe: TypeTree): TermTree

    def apply(expr: tpd.Tree, tpe: Type)(implicit c: Dummy): tpd.Tree
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, Type)]
  }

  def Assign: AssignImpl
  trait AssignImpl {
    def apply(lhs: TermTree, rhs: TermTree): TermTree

    def apply(lhs: tpd.Tree, rhs: tpd.Tree)(implicit c: Dummy): tpd.Tree
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, tpd.Tree)]
  }

  def Update: UpdateImpl
  trait UpdateImpl {
    def apply(fun: TermTree, argss: List[List[TermTree]], rhs: TermTree): TermTree
  }

  def Return: ReturnImpl
  trait ReturnImpl {
    def apply(expr: TermTree): TermTree
    def apply(): TermTree
    def apply(from: Symbol, expr: tpd.Tree): tpd.Tree
    def unapply(tree: tpd.Tree): Option[tpd.Tree]
  }

  def Block: BlockImpl
  trait BlockImpl {
    def apply(stats: List[Tree]): TermTree

    def apply(stats: List[tpd.Tree], expr: tpd.Tree): tpd.Tree
    def unapply(tree: tpd.Tree): Option[(List[tpd.Tree], tpd.Tree)]
  }

  def PartialFunction: PartialFunctionImpl
  trait PartialFunctionImpl {
    def apply(cases: List[Tree]): TermTree
  }

  def Match: MatchImpl
  trait MatchImpl {
    def apply(expr: TermTree, cases: List[Tree]): TermTree
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, List[tpd.Tree])]
  }

  def Case: CaseImpl
  trait CaseImpl {
    def apply(pat: PatTree, cond: Option[TermTree], body: TermTree): Tree
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, Option[tpd.Tree], tpd.Tree)]
  }

  def Tuple: TupleImpl
  trait TupleImpl {
    def apply(args: List[TermTree]): TermTree
  }

  def Interpolate: InterpolateImpl
  trait InterpolateImpl {
    def apply(prefix: String, parts: List[String], args: List[TermTree]): TermTree
  }

  def SeqLiteral: SeqLiteralImpl
  trait SeqLiteralImpl {
    def apply(trees: List[tpd.Tree], tp: Type): tpd.Tree
    def unapply(tree: tpd.Tree): Option[List[tpd.Tree]]
  }

  def TypedSplice: TypedSpliceImpl
  trait TypedSpliceImpl {
    def apply(tree: tpd.Tree): Splice
  }

  def ValDef: ValDefImpl
  trait ValDefImpl {
    def apply(mods: Mods, name: String, tpe: Option[TypeTree], rhs: Tree): ValDef

    def apply(rhs: tpd.Tree, tpOpt: Option[Type] = None, mutable: Boolean = false): tpd.DefTree
    def apply(sym: Symbol, rhs: tpd.Tree): tpd.DefTree
    def unapply(tree: tpd.Tree): Option[(Symbol, tpd.Tree)]
  }

  def ValDecl: ValDeclImpl
  trait ValDeclImpl {
    def apply(mods: Mods, name: String, tpe: TypeTree): ValDecl
  }


  def DefDef: DefDefImpl
  trait DefDefImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], paramss: List[List[Param]], tpe: Option[TypeTree], rhs: Tree): DefDef

    def apply(name: String, tp: MethodType)(body: (Symbol, List[List[tpd.RefTree]]) => tpd.Tree): tpd.DefTree
  }

  def DefDecl: DefDeclImpl
  trait DefDeclImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], paramss: List[List[Param]], tpe: TypeTree): DefDecl
  }

  def Param: ParamImpl
  trait ParamImpl {
    def apply(mods: Mods, name: String, tpe: Option[TypeTree], default: Option[TermTree]): Param
    def apply(name: String): Param = apply(emptyMods, name, None, None)
    def apply(name: String, tpe: TypeTree): Param = apply(emptyMods, name, Some(tpe), None)
  }

  def TypeParam: TypeParamImpl
  trait TypeParamImpl {
    def apply(name: String, tbounds: TypeTree): TypeParam = apply(emptyMods, name, Nil, Some(tbounds), Nil)
    def apply(mods: Mods, name: String, tparams: List[TypeParam], tbounds: Option[TypeTree], cbounds: List[TypeTree]): TypeParam
  }

  def Class: ClassImpl
  trait ClassImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], ctorMods: Mods, paramss: List[List[Param]], parents: List[InitCall], self: Option[Self], stats: List[Tree]): Class
  }

  def Trait: TraitImpl
  trait TraitImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], ctorMods: Mods, paramss: List[List[Param]], parents: List[InitCall], self: Option[Self], stats: List[Tree]): Trait
  }

  def Object: ObjectImpl
  trait ObjectImpl {
    def apply(mods: Mods, name: String, parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Object
  }

  def untpd: untpdImpl
  trait untpdImpl {
    def show(tree: Tree): String

    def traverse(tree: Tree)(pf: PartialFunction[Tree, Unit]): Unit
    def exists(tree: Tree)(pf: PartialFunction[Tree, Boolean]): Boolean
    def transform(tree: Tree)(pf: PartialFunction[Tree, Tree]): Tree
  }

  val tpd: tpdImpl
  trait tpdImpl {
    type Tree      >: Null <: AnyRef
    type DefTree   <: Tree
    type RefTree   <: Tree

    /** type associated with the tree */
    def typeOf(tree: Tree): Type

    def show(tree: Tree): String

    def symbol(tree: Tree): Symbol

    def isDef(tree: Tree): Boolean

    /** subst symbols in tree */
    def subst(tree: Tree)(from: List[Symbol], to: List[Symbol]): Tree

    def traverse(tree: Tree)(pf: PartialFunction[Tree, Unit]): Unit
    def exists(tree: Tree)(pf: PartialFunction[Tree, Boolean]): Boolean
    def transform(tree: Tree)(pf: PartialFunction[Tree, Tree]): Tree
  }
}


