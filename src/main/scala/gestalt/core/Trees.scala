package scala.gestalt.core

trait Positions { this: Trees =>
  // it's safe to assume type trees and untyped trees use the same modelling of position
  type Pos

  def Pos: PosImpl
  trait PosImpl {
    def pos(tree: Tree): Pos
    def pos(tree: tpd.Tree)(implicit c: Cap): Pos
  }
}

trait Trees extends Params with TypeParams with
  ValDefs with ValDecls with DefDefs with DefDecls with
  Classes with Traits with Objects with Positions { toolbox: Toolbox =>

  type Cap >: Null
  implicit val cap: Cap = null

  // safety by construction -- implementation can have TypeTree = Tree
  type Tree     >: Null <: AnyRef
  type TypeTree >: Null <: Tree
  type TermTree >: Null <: Tree
  type DefTree  >: Null <: Tree
  type PatTree  >: Null <: Tree

  type Splice   <: TypeTree with TermTree with DefTree
  type Lit      <: TermTree with PatTree
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
    def withAnnotations(annots: Seq[Tree]): Mods
    def annotations: Seq[Tree]
  }

  // modifiers
  def emptyMods: Mods

  // definition trees
  def NewAnonymClass: NewAnonymClassImpl
  trait NewAnonymClassImpl {
    def apply(parents: Seq[InitCall], self: Option[Self], stats: Seq[Tree]): DefTree
  }

  def TypeDecl: TypeDeclImpl
  trait TypeDeclImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], tbounds: Option[TypeTree]): DefTree
  }

  def TypeAlias: TypeAliasImpl
  trait TypeAliasImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], rhs: TypeTree): DefTree
  }

  def PatDef: PatDefImpl
  trait PatDefImpl {
    def apply(mods: Mods, lhs: PatTree, tpe: Option[TypeTree], rhs: Tree): DefTree
  }

  def SeqDef: SeqDefImpl
  trait SeqDefImpl {
    def apply(mods: Mods, vals: Seq[String], tpe: Option[TypeTree], rhs: Tree): DefTree
  }

  def SeqDecl: SeqDeclImpl
  trait SeqDeclImpl {
    def apply(mods: Mods, vals: Seq[String], tpe: TypeTree): DefTree
  }

  // extends qual.T[A, B](x, y)(z)
  def InitCall: InitCallImpl
  trait InitCallImpl {
    def apply(qual: Option[Tree], name: String, targs: Seq[TypeTree], argss: Seq[Seq[TermTree]]): InitCall
  }

  def NewInstance: NewInstanceImpl
  trait NewInstanceImpl {
    def apply(qual: Option[Tree], name: String, targs: Seq[TypeTree], argss: Seq[Seq[TermTree]]): TermTree
  }

  def SecondaryCtor: SecondaryCtorImpl
  trait SecondaryCtorImpl {
    def apply(mods: Mods, paramss: Seq[Seq[Param]], rhs: TermTree): Tree
  }

  def Self: SelfImpl
  trait SelfImpl {
    def apply(name: String, tpe: TypeTree): Self
    def apply(name: String): Self
  }

  // type trees
  def TypeIdent: TypeIdentImpl
  trait TypeIdentImpl {
    def apply(name: String): TypeTree
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
    def apply(tpe: TypeTree, args: Seq[TypeTree]): TypeTree
  }

  def TypeInfix: TypeInfixImpl
  trait TypeInfixImpl {
    def apply(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree
  }

  def TypeFunction: TypeFunctionImpl
  trait TypeFunctionImpl {
    def apply(params: Seq[TypeTree], res: TypeTree): TypeTree
  }

  def TypeTuple: TypeTupleImpl
  trait TypeTupleImpl {
    def apply(args: Seq[TypeTree]): TypeTree
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
    def apply(stats: Seq[Tree]): TypeTree
    def apply(tpe: TypeTree, stats: Seq[Tree]): TypeTree
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
    def apply(tpe: TypeTree, annots: Seq[Tree]): TypeTree
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
    def apply(expr: TermTree, annots: Seq[Tree]): TermTree
    def unapply(tree: Tree): Option[(TermTree, Seq[Tree])]
  }

  def If: IfImpl
  trait IfImpl {
    def apply(cond: TermTree, thenp: TermTree, elsep: TermTree): TermTree =
      apply(cond, thenp, Some(elsep))

    def apply(cond: TermTree, thenp: TermTree, elsep: Option[TermTree]): TermTree
    def unapply(tree: Tree): Option[(TermTree, TermTree, Option[TermTree])]

    def apply(cond: tpd.Tree, thenp: tpd.Tree, elsep: tpd.Tree)(implicit cap: Cap): tpd.Tree
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[(tpd.Tree, tpd.Tree, tpd.Tree)]
  }

  def Try: TryImpl
  trait TryImpl {
    def apply(expr: Tree, cases: Seq[Tree], finallyp: Option[TermTree]): TermTree
    def apply(expr: TermTree, handler: Tree, finallyp: Option[TermTree]): TermTree
  }

  def Function: FunctionImpl
  trait FunctionImpl {
    def apply(params: Seq[Param], body: TermTree): TermTree
    def apply(params: Seq[(String, Type)], resTp: Type)(bodyFn: Seq[tpd.Tree] => tpd.Tree): tpd.Tree
    def unapply(tree: tpd.Tree): Option[(Seq[Symbol], tpd.Tree)]
  }

  def While: WhileImpl
  trait WhileImpl {
    def apply(expr: TermTree, body: TermTree): TermTree
  }

  def DoWhile: DoWhileImpl
  trait DoWhileImpl {
    def apply(body: TermTree, expr: TermTree): TermTree
  }

  def For: ForImpl
  trait ForImpl {
    def ForDo(enums: Seq[Tree], body: TermTree): TermTree
    def ForYield(enums: Seq[Tree], body: TermTree): TermTree
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
    def Ident(name: String): PatTree = toolbox.Ident(name)
    def Lit(value: Any): PatTree = toolbox.Lit(value)
    def Alt(trees: Seq[PatTree]): PatTree
    def Unapply(fun: TermTree, args: Seq[PatTree]): PatTree
    def Infix(lhs: PatTree, op: String, rhs: PatTree): PatTree
    def Tuple(pats: Seq[PatTree]): PatTree
  }

  // importees
  def Import: ImportImpl
  trait ImportImpl {
    def apply(items: Seq[Tree]): Tree
    def Item(prefix: Tree, importees: Seq[Tree]): Tree
    def Name(name: String): Tree
    def Rename(from: String, to: String): Tree
    def Hide(name: String): Tree
  }

  // extractors
  def Lit: LitImpl
  trait LitImpl {
    def apply(value: Any): Lit
    def unapply(tree: Tree): Option[Any]
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[Any]
  }

  def Apply: ApplyImpl
  trait ApplyImpl {
    def apply(fun: TermTree, args: Seq[TermTree]): TermTree
    def unapply(tree: Tree): Option[(TermTree, Seq[TermTree])]
    def apply(fun: tpd.Tree, args: Seq[tpd.Tree])(implicit c: Cap): tpd.Tree
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[(tpd.Tree, Seq[tpd.Tree])]
  }

  def ApplyType: ApplyTypeImpl
  trait ApplyTypeImpl {
    def apply(fun: TermTree, args: Seq[TypeTree]): TermTree
    def unapply(tree: Tree): Option[(TermTree, Seq[TypeTree])]

    def apply(fun: tpd.Tree, args: Seq[tpd.Tree])(implicit c: Cap): tpd.Tree
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[(tpd.Tree, Seq[tpd.Tree])]
  }

  def Ident: IdentImpl
  trait IdentImpl {
    def apply(name: String): Ident
    def apply(symbol: Symbol): tpd.Tree
    def unapply(tree: Tree): Option[String]
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[String]
  }

  def This: ThisImpl
  trait ThisImpl {
    def apply(qual: String): TermTree
    def unapply(tree: Tree): Option[String]
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[String]
  }

  def Super: SuperImpl
  trait SuperImpl {
    def apply(thisp: String, superp: String): TermTree
  }

  def Select: SelectImpl
  trait SelectImpl {
    def apply(qual: TermTree, name: String): TermTree
    def unapply(tree: Tree): Option[(TermTree, String)]

    def apply(qual: tpd.Tree, name: String)(implicit c: Cap): tpd.Tree
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[(tpd.Tree, String)]
  }

  def Ascribe: AscribeImpl
  trait AscribeImpl {
    def apply(expr: TermTree, tpe: TypeTree): TermTree
    def unapply(tree: Tree): Option[(TermTree, TypeTree)]

    def apply(expr: tpd.Tree, tpe: tpd.Tree)(implicit c: Cap): tpd.Tree
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[(tpd.Tree, tpd.Tree)]
  }

  def Assign: AssignImpl
  trait AssignImpl {
    def apply(lhs: TermTree, rhs: TermTree): TermTree
    def unapply(tree: Tree): Option[(TermTree, TermTree)]
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[(tpd.Tree, tpd.Tree)]
  }

  def Update: UpdateImpl
  trait UpdateImpl {
    def apply(fun: TermTree, argss: Seq[Seq[TermTree]], rhs: TermTree): TermTree
  }

  def Return: ReturnImpl
  trait ReturnImpl {
    def apply(expr: TermTree): TermTree
    def apply: TermTree
    def unapply(tree: Tree): Option[Option[TermTree]]
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[tpd.Tree]
  }

  def Block: BlockImpl
  trait BlockImpl {
    def apply(stats: Seq[Tree]): TermTree
    def unapply(tree: Tree): Option[Seq[Tree]]

    def apply(stats: Seq[tpd.Tree], expr: tpd.Tree)(implicit c: Cap): tpd.Tree
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[(Seq[tpd.Tree], tpd.Tree)]
  }

  def PartialFunction: PartialFunctionImpl
  trait PartialFunctionImpl {
    def apply(cases: Seq[Tree]): TermTree
    def unapply(tree: Tree): Option[Seq[Tree]]
  }

  def Match: MatchImpl
  trait MatchImpl {
    def apply(expr: TermTree, cases: Seq[Tree]): TermTree
    def unapply(tree: Tree): Option[(TermTree, Seq[Tree])]
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[(tpd.Tree, Seq[tpd.Tree])]
  }

  def Case: CaseImpl
  trait CaseImpl {
    def apply(pat: PatTree, cond: Option[TermTree], body: TermTree): Tree
    def unapply(tree: Tree): Option[(TermTree, Option[TermTree], TermTree)]
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[(tpd.Tree, Option[tpd.Tree], tpd.Tree)]
  }

  def Tuple: TupleImpl
  trait TupleImpl {
    def apply(args: Seq[TermTree]): TermTree
    def unapply(tree: Tree): Option[Seq[TermTree]]
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[Seq[tpd.Tree]]
  }

  def Interpolate: InterpolateImpl
  trait InterpolateImpl {
    def apply(prefix: String, parts: Seq[String], args: Seq[TermTree]): TermTree
  }

  def SeqLiteral: SeqLiteralImpl
  trait SeqLiteralImpl {
    def apply(trees: Seq[tpd.Tree], tp: Type): tpd.Tree
    def unapply(tree: tpd.Tree): Option[Seq[tpd.Tree]]
  }

  def TypedSplice: TypedSpliceImpl
  trait TypedSpliceImpl {
    def apply(tree: tpd.Tree): Splice
    def unapply(tree: Tree): Option[tpd.Tree]
  }

  def untpd: untpdImpl
  trait untpdImpl {
    def traverse(tree: Tree)(pf: PartialFunction[Tree, Unit]): Unit
    def exists(tree: Tree)(pf: PartialFunction[Tree, Boolean]): Boolean
    def transform(tree: Tree)(pf: PartialFunction[Tree, Tree]): Tree
  }

  val tpd: tpdImpl
  trait tpdImpl {
    type Tree      >: Null <: AnyRef
    type Param     <: Tree
    type ValDef    <: Tree

    /** type associated with the tree */
    def typeOf(tree: Tree): Type

    /** subst symbols in tree */
    def subst(tree: Tree)(from: List[Symbol], to: List[Symbol]): Tree

    def traverse(tree: Tree)(pf: PartialFunction[Tree, Unit]): Unit
    def exists(tree: Tree)(pf: PartialFunction[Tree, Boolean]): Boolean
    def transform(tree: Tree)(pf: PartialFunction[Tree, Tree]): Tree
  }
}


trait ValDefs { this: Toolbox =>
  def ValDef: ValDefImpl
  trait ValDefImpl {
    def apply(mods: Mods, name: String, tpe: Option[TypeTree], rhs: Tree): ValDef
    def mods(tree: ValDef): Mods
    def name(tree: ValDef): String
    def rhs(tree: ValDef): TermTree
    def tptOpt(tree: ValDef): Option[TypeTree]
    def copyRhs(tree: ValDef)(rhs: TermTree): ValDef
    def get(tree: Tree): Option[ValDef]
    def unapply(tree: Tree): Option[(String, Option[TypeTree], TermTree)]

    def apply(name: String, rhs: tpd.Tree): tpd.ValDef
    def symbol(tree: tpd.ValDef)(implicit c: Cap): Symbol
    def name(tree: tpd.ValDef)(implicit c: Cap): String
    def rhs(tree: tpd.ValDef)(implicit c: Cap): TermTree
    def tptOpt(tree: tpd.ValDef)(implicit c: Cap): Option[TypeTree]
    def copyRhs(tree: tpd.ValDef)(rhs: tpd.Tree)(implicit c: Cap): tpd.ValDef
    def get(tree: tpd.Tree)(implicit c: Cap): Option[tpd.ValDef]
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[(String, Option[TypeTree], TermTree)]
  }
}

trait ValDecls { this: Trees =>

  def ValDecl: ValDeclImpl
  trait ValDeclImpl {
    def apply(mods: Mods, name: String, tpe: TypeTree): ValDecl
    def mods(tree: ValDecl): Mods
    def name(tree: ValDecl): String
    def tpt(tree: ValDecl): TypeTree
    def get(tree: Tree): Option[ValDecl]
    def unapply(tree: Tree): Option[(Mods, String, TypeTree)]
  }

}

trait DefDefs { this: Trees =>

  def DefDef: DefDefImpl
  trait DefDefImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], paramss: Seq[Seq[Param]], tpe: Option[TypeTree], rhs: Tree): DefDef
    def mods(tree: DefDef): Mods
    def tparams(tree: DefDef): Seq[TypeParam]
    def paramss(tree: DefDef): Seq[Seq[Param]]
    def name(tree: DefDef): String
    def tptOpt(tree: DefDef): Option[TypeTree]
    def rhs(tree: DefDef): TermTree
    def copyRhs(tree: DefDef)(rhs: TermTree): DefDef
    def get(tree: Tree): Option[DefDef]
    def unapply(tree: Tree): Option[(Mods, String, Seq[TypeParam], Seq[Seq[Param]], Option[TypeTree], TermTree)]
  }

}

trait DefDecls { this: Trees =>

  def DefDecl: DefDeclImpl
  trait DefDeclImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], paramss: Seq[Seq[Param]], tpe: TypeTree): DefDecl
    def mods(tree: DefDecl): Mods
    def tparams(tree: DefDecl): Seq[TypeParam]
    def paramss(tree: DefDecl): Seq[Seq[Param]]
    def name(tree: DefDecl): String
    def tpt(tree: DefDecl): TypeTree
    def get(tree: Tree): Option[DefDecl]
    def unapply(tree: Tree): Option[(Mods, String, Seq[TypeParam], Seq[Seq[Param]], TypeTree)]
  }

}

trait Params { self : Toolbox =>
  def Param: ParamImpl
  trait ParamImpl {
    def apply(name: String): Param = apply(emptyMods, name, None, None)
    def apply(name: String, tpe: TypeTree): Param = apply(emptyMods, name, Some(tpe), None)

    def apply(mods: Mods, name: String, tpe: Option[TypeTree], default: Option[TermTree]): Param
    def mods(tree: Param): Mods
    def name(tree: Param): String
    def tptOpt(tree: Param): Option[TypeTree]
    def defaultOpt(tree: Param): Option[TermTree]
    def copyMods(tree: Param)(mods: Mods): Param

    def symbol(tree: tpd.Param)(implicit c: Cap): Symbol
    def name(tree: tpd.Param)(implicit c: Cap): String
    def tpt(tree: tpd.Param)(implicit c: Cap): tpd.Tree
  }
}

trait TypeParams { this: Trees =>

  def TypeParam: TypeParamImpl
  trait TypeParamImpl {
    def apply(name: String, tbounds: TypeTree): TypeParam = apply(emptyMods, name, Nil, Some(tbounds), Nil)
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], tbounds: Option[TypeTree], cbounds: Seq[TypeTree]): TypeParam
    def mods(tree: TypeParam): Mods
    def name(tree: TypeParam): String
    def tparams(tree: TypeParam): Seq[TypeParam]
  }
}

trait Classes { this: Trees =>

  def Class: ClassImpl
  trait ClassImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], ctorMods: Mods, paramss: Seq[Seq[Param]], parents: Seq[InitCall], self: Option[Self], stats: Seq[Tree]): Class
    def mods(tree: Class): Mods
    def name(tree: Class): String
    def ctorMods(tree: Class): Mods
    def tparams(tree: Class): Seq[TypeParam]
    def paramss(tree: Class): Seq[Seq[Param]]
    def parents(tree: Class): Seq[InitCall]
    def selfOpt(tree: Class): Option[Self]
    def stats(tree: Class): Seq[Tree]
    def copyMods(tree: Class)(mods: Mods): Class
    def copyParamss(tree: Class)(paramss: Seq[Seq[Param]]): Class
    def copyStats(tree: Class)(stats: Seq[Tree]): Class
    def get(tree: Tree): Option[Class]
    def unapply(tree: Tree): Option[(Mods, String, Seq[TypeParam], Mods, Seq[Seq[Param]], Seq[InitCall], Option[Self], Seq[Tree])]
  }
}

trait Traits { this: Trees =>

  def Trait: TraitImpl
  trait TraitImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], ctorMods: Mods, paramss: Seq[Seq[Param]], parents: Seq[InitCall], self: Option[Self], stats: Seq[Tree]): Trait
    def mods(tree: Trait): Mods
    def name(tree: Trait): String
    def tparams(tree: Trait): Seq[TypeParam]
    def paramss(tree: Trait): Seq[Seq[Param]]
    def parents(tree: Trait): Seq[InitCall]
    def selfOpt(tree: Trait): Option[Self]
    def stats(tree: Trait): Seq[Tree]
    def copyStats(tree: Trait)(stats: Seq[Tree]): Trait
    def get(tree: Tree): Option[Trait]
    def unapply(tree: Tree): Option[(Mods, String, Seq[TypeParam], Mods, Seq[Seq[Param]], Seq[InitCall], Option[Self], Seq[Tree])]
  }
}

trait Objects { this: Trees =>
  def Object: ObjectImpl
  trait ObjectImpl {
    def apply(mods: Mods, name: String, parents: Seq[InitCall], selfOpt: Option[Self], stats: Seq[Tree]): Object
    def mods(tree: Object): Mods
    def name(tree: Object): String
    def parents(tree: Object): Seq[InitCall]
    def selfOpt(tree: Object): Option[Self]
    def stats(tree: Object): Seq[Tree]
    // separate copy for better versioning compatibility
    def copyStats(tree: Object)(stats: Seq[Tree]): Object
    def get(tree: Tree): Option[Object]
    def unapply(tree: Tree): Option[(Mods, String, Seq[InitCall], Option[Self], Seq[Tree])]
  }
}
