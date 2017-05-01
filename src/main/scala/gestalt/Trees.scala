package scala.gestalt

trait Trees extends Params with TypeParams with
  ValDefs with ValDecls with DefDefs with DefDecls with
  Classes with Traits with Objects {
  // safety by construction -- implementation can have TypeTree = Tree
  type Tree     >: Null <: AnyRef
  type TypeTree >: Null <: Tree
  type TermTree >: Null <: Tree
  type DefTree  >: Null <: Tree
  type Splice   <: TypeTree with TermTree with DefTree

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
  val AnonymClass: AnonymClassImpl
  trait AnonymClassImpl {
    def apply(parents: Seq[InitCall], self: Option[Self], stats: Seq[Tree]): DefTree
  }

  val TypeDecl: TypeDeclImpl
  trait TypeDeclImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], tbounds: Option[TypeTree]): DefTree
  }

  val TypeAlias: TypeAliasImpl
  trait TypeAliasImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], rhs: TypeTree): DefTree
  }

  val PatDef: PatDefImpl
  trait PatDefImpl {
    def apply(mods: Mods, lhs: TermTree, tpe: Option[TypeTree], rhs: Tree): DefTree
  }

  val SeqDef: SeqDefImpl
  trait SeqDefImpl {
    def apply(mods: Mods, pats: Seq[TermTree], tpe: Option[TypeTree], rhs: Tree): DefTree
  }

  val SeqDecl: SeqDeclImpl
  trait SeqDeclImpl {
    def apply(mods: Mods, vals: Seq[String], tpe: TypeTree): DefTree
  }

  // extends qual.T[A, B](x, y)(z)
  val InitCall: InitCallImpl
  trait InitCallImpl {
    def apply(qual: Option[Tree], name: String, targs: Seq[TypeTree], argss: Seq[Seq[TermTree]]): InitCall
  }

  val SecondaryCtor: SecondaryCtorImpl
  trait SecondaryCtorImpl {
    def apply(mods: Mods, paramss: Seq[Seq[Param]], rhs: TermTree): Tree
  }

  val Self: SelfImpl
  trait SelfImpl {
    def apply(name: String, tpe: TypeTree): Self
    def apply(name: String): Self
  }

  // type trees
  val TypeIdent: TypeIdentImpl
  trait TypeIdentImpl {
    def apply(name: String): TypeTree
  }

  val TypeSelect: TypeSelectImpl
  trait TypeSelectImpl {
    def apply(qual: Tree, name: String): TypeTree
  }

  val TypeSingleton: TypeSingletonImpl
  trait TypeSingletonImpl {
    def apply(ref: Tree): TypeTree
  }

  val TypeApply: TypeApplyImpl
  trait TypeApplyImpl {
    def apply(tpe: TypeTree, args: Seq[TypeTree]): TypeTree
  }

  val TypeApplyInfix: TypeApplyInfixImpl
  trait TypeApplyInfixImpl {
    def apply(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree
  }

  val TypeFunction: TypeFunctionImpl
  trait TypeFunctionImpl {
    def apply(params: Seq[TypeTree], res: TypeTree): TypeTree
  }

  val TypeTuple: TypeTupleImpl
  trait TypeTupleImpl {
    def apply(args: Seq[TypeTree]): TypeTree
  }

  val TypeAnd: TypeAndImpl
  trait TypeAndImpl {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree
  }

  val TypeOr: TypeOrImpl
  trait TypeOrImpl {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree
  }

  val TypeRefine: TypeRefineImpl
  trait TypeRefineImpl {
    def apply(tpe: Option[TypeTree], stats: Seq[Tree]): TypeTree
  }

  val TypeBounds: TypeBoundsImpl
  trait TypeBoundsImpl {
    def apply(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree
  }

  val TypeRepeated: TypeRepeatedImpl
  trait TypeRepeatedImpl {
    def apply(tpe: TypeTree): TypeTree
  }

  val TypeByName: TypeByNameImpl
  trait TypeByNameImpl {
    def apply(tpe: TypeTree): TypeTree
  }

  val TypeAnnotated: TypeAnnotatedImpl
  trait TypeAnnotatedImpl {
    def apply(tpe: TypeTree, annots: Seq[Tree]): TypeTree
  }

  // terms
  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  val Infix: InfixImpl
  trait InfixImpl {
    def apply(lhs: TermTree, op: String, rhs: TermTree): TermTree
  }

  val Prefix: PrefixImpl
  trait PrefixImpl {
    def apply(op: String, od: TermTree): TermTree
  }

  val Postfix: PostfixImpl
  trait PostfixImpl {
    def apply(od: TermTree, op: String): TermTree
  }

  val Throw: ThrowImpl
  trait ThrowImpl {
    def apply(expr: TermTree): TermTree
  }

  val Annotated: AnnotatedImpl
  trait AnnotatedImpl {
    def apply(expr: TermTree, annots: Seq[Tree]): TermTree
    def unapply(tree: Tree): Option[(TermTree, Seq[Tree])]
  }

  val If: IfImpl
  trait IfImpl {
    def apply(cond: TermTree, thenp: TermTree, elsep: Option[TermTree]): TermTree
    def unapply(tree: Tree): Option[(TermTree, TermTree, Option[TermTree])]
  }

  val Try: TryImpl
  trait TryImpl {
    def apply(expr: Tree, cases: Seq[Tree], finallyp: Option[TermTree]): TermTree
    def apply(expr: TermTree, handler: Tree, finallyp: Option[TermTree]): TermTree
  }

  val Function: FunctionImpl
  trait FunctionImpl {
    def apply(params: Seq[Param], body: TermTree): TermTree
  }

  val While: WhileImpl
  trait WhileImpl {
    def apply(expr: TermTree, body: TermTree): TermTree
  }

  val DoWhile: DoWhileImpl
  trait DoWhileImpl {
    def apply(body: TermTree, expr: TermTree): TermTree
  }

  val For: ForImpl
  trait ForImpl {
    def ForDo(enums: Seq[Tree], body: TermTree): TermTree
    def ForYield(enums: Seq[Tree], body: TermTree): TermTree
    def GenFrom(pat: TermTree, rhs: TermTree): Tree
    def GenAlias(pat: TermTree, rhs: TermTree): Tree
    def Guard(cond: TermTree): Tree
  }

  val New: NewImpl
  trait NewImpl {
    // can be InitCall or AnonymClass
    def apply(tpe: Tree): TermTree
  }

  val Named: NamedImpl
  trait NamedImpl {
    def apply(name: String, expr: Tree): TermTree
  }

  val Repeated: RepeatedImpl
  trait RepeatedImpl {
    def apply(expr: Tree): TermTree
  }

  // patterns
  val Bind: BindImpl
  trait BindImpl {
    def apply(name: String, expr: TermTree): TermTree
  }

  val Alternative: AlternativeImpl
  trait AlternativeImpl {
    def apply(trees: Seq[TermTree]): TermTree
  }

  // importees
  val Import: ImportImpl
  trait ImportImpl {
    def apply(items: Seq[Tree]): Tree
    def Item(prefix: Tree, importees: Seq[Tree]): Tree
    def Name(name: String): Tree
    def Rename(from: String, to: String): Tree
    def Hide(name: String): Tree
  }

  // extractors
  val Lit: LitImpl
  trait LitImpl {
    def apply(value: Any): TermTree
    def unapply(tree: Tree): Option[Any]
  }

  val Apply: ApplyImpl
  trait ApplyImpl {
    def apply(fun: TermTree, args: Seq[TermTree]): TermTree
    def unapply(tree: Tree): Option[(TermTree, Seq[TermTree])]
  }

  val ApplyType: ApplyTypeImpl
  trait ApplyTypeImpl {
    def apply(fun: TermTree, args: Seq[TypeTree]): TermTree
    def unapply(tree: Tree): Option[(TermTree, Seq[TypeTree])]
  }

  val Ident: IdentImpl
  trait IdentImpl {
    def apply(name: String): TermTree
    def unapply(tree: Tree): Option[String]
  }

  val This: ThisImpl
  trait ThisImpl {
    def apply(qual: String): TermTree
    def unapply(tree: Tree): Option[String]
  }

  val Super: SuperImpl
  trait SuperImpl {
    def apply(thisp: String, superp: String): TermTree
  }

  val Select: SelectImpl
  trait SelectImpl {
    def apply(qual: TermTree, name: String): TermTree
    def unapply(tree: Tree): Option[(TermTree, String)]
  }

  val Ascribe: AscribeImpl
  trait AscribeImpl {
    def apply(expr: TermTree, tpe: TypeTree): TermTree
    def unapply(tree: Tree): Option[(TermTree, TypeTree)]
  }

  val Assign: AssignImpl
  trait AssignImpl {
    def apply(lhs: TermTree, rhs: TermTree): TermTree
    def unapply(tree: Tree): Option[(TermTree, TermTree)]
  }


  val Return: ReturnImpl
  trait ReturnImpl {
    def apply(expr: TermTree): TermTree
    def apply: TermTree
    def unapply(tree: Tree): Option[Option[TermTree]]
  }

  val Block: BlockImpl
  trait BlockImpl {
    def apply(stats: Seq[Tree]): TermTree
    def unapply(tree: Tree): Option[Seq[Tree]]
  }

  val PartialFunction: PartialFunctionImpl
  trait PartialFunctionImpl {
    def apply(cases: Seq[Tree]): TermTree
    def unapply(tree: Tree): Option[Seq[Tree]]
  }

  val Match: MatchImpl
  trait MatchImpl {
    def apply(expr: TermTree, cases: Seq[Tree]): TermTree
    def unapply(tree: Tree): Option[(TermTree, Seq[Tree])]
  }

  val Case: CaseImpl
  trait CaseImpl {
    def apply(pat: TermTree, cond: Option[TermTree], body: TermTree): Tree
    def unapply(tree: Tree): Option[(TermTree, Option[TermTree], TermTree)]
  }

  val Tuple: TupleImpl
  trait TupleImpl {
    def apply(args: Seq[TermTree]): TermTree
    def unapply(tree: Tree): Option[Seq[TermTree]]
  }

  val Interpolate: InterpolateImpl
  trait InterpolateImpl {
    def apply(prefix: String, parts: Seq[String], args: Seq[TermTree]): TermTree
  }

  val SeqLiteral: SeqLiteralImpl
  trait SeqLiteralImpl {
    def unapply(tree: Tree): Option[Seq[TermTree]]
  }

  val TypedSplice: TypedSpliceImpl
  trait TypedSpliceImpl {
    def apply(tree: Tree): Splice
  }

  // helper
  def ApplySeq(fun: TermTree, argss: Seq[Seq[TermTree]]): Tree = argss match {
   case args :: rest => rest.foldLeft(Apply(fun, args)) { (acc, args) => Apply(acc, args) }
   case _ => Apply(fun, Nil)
  }

  object ApplySeq {
    def unapply(call: TermTree): Option[(Tree, Seq[Seq[TermTree]])] = {
      def recur(acc: Seq[Seq[TermTree]], term: TermTree): (TermTree, Seq[Seq[TermTree]]) = term match {
        case Apply(fun, args) => recur(args +: acc, fun) // inner-most is in the front
        case fun => (fun, acc)

      }

      Some(recur(Nil, call))
    }
  }

  object Applysss {
    def unapply(tree: TermTree): Option[(TermTree, Seq[TypeTree], Seq[Seq[TermTree]])] = tree match {
      case ApplyType(fun, targs) =>
        Some((fun, targs, Nil))
      case Apply(fun, args) =>
        val Some((f, targs, argss)) = unapply(fun)
        Some((f, targs, argss :+ args))
      case _ =>
        Some((tree, Nil, Nil))
    }
  }

  // traverser
  def traverse(tre: Tree)(pf: PartialFunction[Tree, Unit]): Unit
  def exists(tree: Tree)(pf: PartialFunction[Tree, Boolean]): Boolean
  def transform(tree: Tree)(pf: PartialFunction[Tree, Tree]): Tree
}

trait ValDefs { this: Trees =>
  implicit class ValDefOps(tree: ValDef) {
    def mods: Mods = ValDef.mods(tree)
    def name: String = ValDef.name(tree)
    def tptOpt: Option[TypeTree] = ValDef.tptOpt(tree)
    def rhs: TermTree = ValDef.rhs(tree)
    def copy(rhs: TermTree = this.rhs): ValDef = ValDef.copyRhs(tree)(rhs)
  }

  val ValDef: ValDefImpl
  trait ValDefImpl {
    def apply(mods: Mods, name: String, tpe: Option[TypeTree], rhs: Tree): ValDef
    def mods(tree: ValDef): Mods
    def name(tree: ValDef): String
    def rhs(tree: ValDef): TermTree
    def tptOpt(tree: ValDef): Option[TypeTree]
    def copyRhs(tree: ValDef)(rhs: TermTree): ValDef
    def get(tree: Tree): Option[ValDef]
    def unapply(tree: Tree): Option[(Mods, String, Option[TypeTree], TermTree)]
  }

  object OfValDef {
    def unapply(tree: Tree): Option[ValDef] = ValDef.get(tree)
  }
}

trait ValDecls { this: Trees =>
 implicit class ValDeclOps(tree: ValDecl) {
    def mods: Mods = ValDecl.mods(tree)
    def name: String = ValDecl.name(tree)
    def tpt: TypeTree = ValDecl.tpt(tree)
  }

  val ValDecl: ValDeclImpl
  trait ValDeclImpl {
    def apply(mods: Mods, name: String, tpe: TypeTree): ValDecl
    def mods(tree: ValDecl): Mods
    def name(tree: ValDecl): String
    def tpt(tree: ValDecl): TypeTree
    def get(tree: Tree): Option[ValDecl]
    def unapply(tree: Tree): Option[(Mods, String, TypeTree)]
  }

  object OfValDecl {
    def unapply(tree: Tree): Option[ValDecl] = ValDecl.get(tree)
  }
}

trait DefDefs { this: Trees =>
  implicit class DefDefOps(tree: DefDef) {
    def mods: Mods = DefDef.mods(tree)
    def tparams: Seq[TypeParam] = DefDef.tparams(tree)
    def paramss: Seq[Seq[Param]] = DefDef.paramss(tree)
    def name: String = DefDef.name(tree)
    def tptOpt: Option[TypeTree] = DefDef.tptOpt(tree)
    def rhs: Tree = DefDef.rhs(tree)
    def copy(rhs: Tree = this.rhs): DefDef = DefDef.copyRhs(tree)(rhs)
  }

  val DefDef: DefDefImpl
  trait DefDefImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], paramss: Seq[Seq[Param]], tpe: Option[TypeTree], rhs: Tree): DefDef
    def mods(tree: DefDef): Mods
    def tparams(tree: DefDef): Seq[TypeParam]
    def paramss(tree: DefDef): Seq[Seq[Param]]
    def name(tree: DefDef): String
    def tptOpt(tree: DefDef): Option[TypeTree]
    def rhs(tree: DefDef): TermTree
    def copyRhs(tree: DefDef)(rhs: Tree): DefDef
    def get(tree: Tree): Option[DefDef]
    def unapply(tree: Tree): Option[(Mods, String, Seq[TypeParam], Seq[Seq[Param]], Option[TypeTree], TermTree)]
  }

  object OfDefDef {
    def unapply(tree: Tree): Option[DefDef] = DefDef.get(tree)
  }
}

trait DefDecls { this: Trees =>
  implicit class DefDeclOps(tree: DefDecl) {
    def mods: Mods = DefDecl.mods(tree)
    def tparams: Seq[TypeParam] = DefDecl.tparams(tree)
    def paramss: Seq[Seq[Param]] = DefDecl.paramss(tree)
    def name: String = DefDecl.name(tree)
    def tpt: TypeTree = DefDecl.tpt(tree)
  }

  val DefDecl: DefDeclImpl
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

  object OfDefDecl {
    def unapply(tree: Tree): Option[DefDecl] = DefDecl.get(tree)
  }
}

trait Params { this: Trees =>
  implicit class ParamOps(tree: Param) {
    def mods: Mods = Param.mods(tree)
    def name: String = Param.name(tree)
    def tptOpt: Option[TypeTree] = Param.tptOpt(tree)
    def defaultOpt: Option[TermTree] = Param.defaultOpt(tree)
    def copy(mods: Mods = this.mods): Param
    = Param.copyMods(tree)(mods)
  }

  val Param: ParamImpl
  trait ParamImpl {
    def apply(mods: Mods, name: String, tpe: Option[TypeTree], default: Option[TermTree]): Param
    def mods(tree: Param): Mods
    def name(tree: Param): String
    def tptOpt(tree: Param): Option[TypeTree]
    def defaultOpt(tree: Param): Option[TermTree]
    def copyMods(tree: Param)(mods: Mods): Param
  }
}

trait TypeParams { this: Trees =>
  implicit class TypeParamOps(tree: TypeParam) {
    def mods: Mods = TypeParam.mods(tree)
    def name: String = TypeParam.name(tree)
    def tparams: Seq[TypeParam] = TypeParam.tparams(tree)
  }

  val TypeParam: TypeParamImpl
  trait TypeParamImpl {
    def apply(mods: Mods, name: String, tparams: Seq[TypeParam], tbounds: Option[TypeTree], cbounds: Seq[TypeTree]): TypeParam
    def mods(tree: TypeParam): Mods
    def name(tree: TypeParam): String
    def tparams(tree: TypeParam): Seq[TypeParam]
  }
}

trait Classes { this: Trees =>
  implicit def ClassOps(tree: Class) {
    def mods: Mods = Class.mods(tree)
    def ctorMods: Mods = Class.ctorMods(tree)
    def name: String = Class.name(tree)
    def tparams: Seq[TypeParam] = Class.tparams(tree)
    def paramss: Seq[Seq[Param]] = Class.paramss(tree)
    def parents: Seq[InitCall] = Class.parents(tree)
    def selfOpt: Option[Self] = Class.selfOpt(tree)
    def stats: Seq[Tree] = Class.stats(tree)
    def copy(mods: Mods = Class.mods(tree), paramss: Seq[Seq[Param]] = Class.paramss(tree), stats: Seq[Tree] = Class.stats(tree)): Class
    = {
      val tree1 = Class.copyMods(tree)(mods)
      val tree2 = Class.copyParamss(tree1)(paramss)
      Class.copyStats(tree2)(stats)
    }
  }

  val Class: ClassImpl
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

  object OfClass {
    def unapply(tree: Tree): Option[Class] = Class.get(tree)
  }
}

trait Traits { this: Trees =>
  implicit def TraitOps(tree: Trait) {
    def mods: Mods = Trait.mods(tree)
    def name: String = Trait.name(tree)
    def tparams: Seq[TypeParam] = Trait.tparams(tree)
    def paramss: Seq[Seq[Param]] = Trait.paramss(tree)
    def parents: Seq[InitCall] = Trait.parents(tree)
    def selfOpt: Option[Self] = Trait.selfOpt(tree)
    def stats: Seq[Tree] = Trait.stats(tree)
    def copy(stats: Seq[Tree] = Trait.stats(tree)): Trait
    = Trait.copyStats(tree)(stats)
  }

  val Trait: TraitImpl
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

  object OfTrait {
    def unapply(tree: Tree): Option[Trait] = Trait.get(tree)
  }
}

trait Objects { this: Trees =>
   implicit def ObjectOps(tree: Object) {
    def mods: Mods = Object.mods(tree)
    def name: String = Object.name(tree)
    def parents: Seq[InitCall] = Object.parents(tree)
    def selfOpt: Option[Self] = Object.selfOpt(tree)
    def stats: Seq[Tree] = Object.stats(tree)
    def copy(stats: Seq[Tree] = Object.stats(tree)): Object
    = Object.copyStats(tree)(stats)
  }

  val Object: ObjectImpl
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

  object OfObject {
    def unapply(tree: Tree): Option[Object] = Object.get(tree)
  }
}

