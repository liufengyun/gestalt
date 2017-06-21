package scala.gestalt

import scala.gestalt.core._

// package object causes compilation errors
object api extends Toolbox {
  /** The marker for meta block to highlight the different semantics */
  def meta(body: Tree): Nothing = ???

  /**------------------------------------------------*/
  private val toolboxStore: ThreadLocal[Toolbox] = new ThreadLocal[Toolbox]

  @inline private def toolbox = toolboxStore.get

  def withToolbox[T](tb: Toolbox)(f: => T): T = {
    toolboxStore.set(tb)
    val res = f
    toolboxStore.remove()

    res
  }

  /**------------------------------------------------*/
  type Unsafe >: Null
  type Location = core.Location

  def emptyMods: Mods = toolbox.emptyMods.asInstanceOf[Mods]

  /**------------------------------------------------*/
  // diagnostics
  def location: Location = toolbox.location

  def error(message: String, pos: Pos) = {
    val tb = toolbox
    tb.error(message, pos.asInstanceOf[tb.Pos])
  }

  /** stop macro transform */
  def abort(message: String, pos: Pos): Nothing = {
    val tb = toolbox
    tb.abort(message, pos.asInstanceOf[tb.Pos])
  }

  /**------------------------------------------------*/
  /** generate fresh unique name */
  def fresh(prefix: String = "$local"): String  = toolbox.fresh(prefix)

  /** The root package
   *
   *  For hygiene, by default only fully-qualified names are allowed, unless
   *  `scala.gestalt.options.unsafe` is imported.
   */
  def root: TermTree = Ident("_root_")
  def empty: TermTree = Ident("_empty_")

  def Ident(name: "_root_"): TermTree = {
    import options.unsafe
    Ident.apply("_root_")
  }

  def Ident(name: "scala")(implicit dummy: Dummy): TermTree = {
    import options.unsafe
    Ident.apply("scala")
  }

  def Ident(name: "java")(implicit dummy: Dummy1): TermTree = {
    import options.unsafe
    Ident.apply("java")
  }

  def Ident(name: "_empty_")(implicit dummy: Dummy2): TermTree = {
    import options.unsafe
    Ident.apply("<empty>")
  }

  /**------------------------------------------------*/
  // definitions
  def NewAnonymClass = toolbox.NewAnonymClass.asInstanceOf[NewAnonymClassImpl]
  def TypeDecl       = toolbox.TypeDecl.asInstanceOf[TypeDeclImpl]
  def TypeAlias      = toolbox.TypeAlias.asInstanceOf[TypeAliasImpl]
  def PatDef         = toolbox.PatDef.asInstanceOf[PatDefImpl]
  def SeqDef         = toolbox.SeqDef.asInstanceOf[SeqDefImpl]
  def SeqDecl        = toolbox.SeqDecl.asInstanceOf[SeqDeclImpl]
  def InitCall       = toolbox.InitCall.asInstanceOf[InitCallImpl]
  def NewInstance    = toolbox.NewInstance.asInstanceOf[NewInstanceImpl]
  def SecondaryCtor  = toolbox.SecondaryCtor.asInstanceOf[SecondaryCtorImpl]
  def Self           = toolbox.Self.asInstanceOf[SelfImpl]

  // type trees
  def TypeIdent      = toolbox.TypeIdent.asInstanceOf[TypeIdentImpl]
  def TypeSelect     = toolbox.TypeSelect.asInstanceOf[TypeSelectImpl]
  def TypeSingleton  = toolbox.TypeSingleton.asInstanceOf[TypeSingletonImpl]
  def TypeApply      = toolbox.TypeApply.asInstanceOf[TypeApplyImpl]
  def TypeInfix      = toolbox.TypeInfix.asInstanceOf[TypeInfixImpl]
  def TypeFunction   = toolbox.TypeFunction.asInstanceOf[TypeFunctionImpl]
  def TypeTuple      = toolbox.TypeTuple.asInstanceOf[TypeTupleImpl]
  def TypeAnd        = toolbox.TypeAnd.asInstanceOf[TypeAndImpl]
  def TypeOr         = toolbox.TypeOr.asInstanceOf[TypeOrImpl]
  def TypeRefine     = toolbox.TypeRefine.asInstanceOf[TypeRefineImpl]
  def TypeBounds     = toolbox.TypeBounds.asInstanceOf[TypeBoundsImpl]
  def TypeRepeated   = toolbox.TypeRepeated.asInstanceOf[TypeRepeatedImpl]
  def TypeByName     = toolbox.TypeByName.asInstanceOf[TypeByNameImpl]
  def TypeAnnotated  = toolbox.TypeAnnotated.asInstanceOf[TypeAnnotatedImpl]

  // terms
  def Infix           = toolbox.Infix.asInstanceOf[InfixImpl]
  def Prefix          = toolbox.Prefix.asInstanceOf[PrefixImpl]
  def Postfix         = toolbox.Postfix.asInstanceOf[PostfixImpl]
  def Throw           = toolbox.Throw.asInstanceOf[ThrowImpl]
  def Annotated       = toolbox.Annotated.asInstanceOf[AnnotatedImpl]
  def If              = toolbox.If.asInstanceOf[IfImpl]
  def Try             = toolbox.Try.asInstanceOf[TryImpl]
  def Function        = toolbox.Function.asInstanceOf[FunctionImpl]
  def While           = toolbox.While.asInstanceOf[WhileImpl]
  def DoWhile         = toolbox.DoWhile.asInstanceOf[DoWhileImpl]
  def For             = toolbox.For.asInstanceOf[ForImpl]
  def Named           = toolbox.Named.asInstanceOf[NamedImpl]
  def Repeated        = toolbox.Repeated.asInstanceOf[RepeatedImpl]
  def Lit             = toolbox.Lit.asInstanceOf[LitImpl]
  def Apply           = toolbox.Apply.asInstanceOf[ApplyImpl]
  def ApplyType       = toolbox.ApplyType.asInstanceOf[ApplyTypeImpl]
  def Ident           = toolbox.Ident.asInstanceOf[IdentImpl]
  def This            = toolbox.This.asInstanceOf[ThisImpl]
  def Super           = toolbox.Super.asInstanceOf[SuperImpl]
  def Select          = toolbox.Select.asInstanceOf[SelectImpl]
  def Ascribe         = toolbox.Ascribe.asInstanceOf[AscribeImpl]
  def Assign          = toolbox.Assign.asInstanceOf[AssignImpl]
  def Update          = toolbox.Update.asInstanceOf[UpdateImpl]
  def Return          = toolbox.Return.asInstanceOf[ReturnImpl]
  def Block           = toolbox.Block.asInstanceOf[BlockImpl]
  def PartialFunction = toolbox.PartialFunction.asInstanceOf[PartialFunctionImpl]
  def Match           = toolbox.Match.asInstanceOf[MatchImpl]
  def Case            = toolbox.Case.asInstanceOf[CaseImpl]
  def Tuple           = toolbox.Tuple.asInstanceOf[TupleImpl]
  def Interpolate     = toolbox.Interpolate.asInstanceOf[InterpolateImpl]
  def SeqLiteral      = toolbox.SeqLiteral.asInstanceOf[SeqLiteralImpl]
  def TypedSplice     = toolbox.TypedSplice.asInstanceOf[TypedSpliceImpl]


  def Import         = toolbox.Import.asInstanceOf[ImportImpl]
  def Pat            = toolbox.Pat.asInstanceOf[PatImpl]

  /**--------------------- Positions ---------------------------------*/
  def Pos            = toolbox.Pos.asInstanceOf[PosImpl]

  /**--------------------- TreeOps ---------------------------------*/
  def untpd          = toolbox.untpd.asInstanceOf[untpdImpl]

  implicit class UntypedTreeOps(tree: Tree) {
    def pos: Pos = Pos.pos(tree)

    def traverse(pf: PartialFunction[Tree, Unit]): Unit =
      untpd.traverse(tree)(pf)

    def exists(pf: PartialFunction[Tree, Boolean]): Boolean =
      untpd.exists(tree)(pf)

    def transform(pf: PartialFunction[Tree, Tree]): Tree =
      untpd.transform(tree)(pf)
  }

  implicit class UntypedTermTreeOps(tree: TermTree) {
    def appliedTo(args: TermTree*): TermTree = Apply(tree, args.toList)
    def selectType(path: String): TypeTree = {
      val parts = path.split('.')

      val prefix = parts.init.foldLeft[TermTree](tree) { (prefix, name) =>
        Select(prefix, name)
      }

      TypeSelect(prefix, parts.last)
    }

    def select(path: String): TermTree = {
      val parts = path.split('.')
      parts.foldLeft[TermTree](tree) { (prefix, name) =>
        Select(prefix, name)
      }
    }
  }

  object tpd extends tpdImpl {
    def typeOf(tree: Tree): Type = {
      val tb = toolbox
      tb.tpd.typeOf(tree.asInstanceOf[tb.tpd.Tree]).asInstanceOf[Type]
    }

    def subst(tree: Tree)(from: List[Symbol], to: List[Symbol]): Tree = {
      val tb = toolbox
      tb.tpd.subst(tree.asInstanceOf[tb.tpd.Tree])(
        from.asInstanceOf[List[tb.Symbol]],
        to.asInstanceOf[List[tb.Symbol]]
      ).asInstanceOf[Tree]
    }

    def traverse(tree: Tree)(pf: PartialFunction[Tree, Unit]): Unit = {
      val tb = toolbox
      tb.tpd.traverse(tree.asInstanceOf[tb.tpd.Tree])(
        pf.asInstanceOf[PartialFunction[tb.tpd.Tree, Unit]]
      )
    }

    def exists(tree: Tree)(pf: PartialFunction[Tree, Boolean]): Boolean = {
      val tb = toolbox
      tb.tpd.exists(tree.asInstanceOf[tb.tpd.Tree])(
        pf.asInstanceOf[PartialFunction[tb.tpd.Tree, Boolean]]
      )
    }

    def transform(tree: Tree)(pf: PartialFunction[Tree, Tree]): Tree = {
      val tb = toolbox
      tb.tpd.transform(tree.asInstanceOf[tb.tpd.Tree])(
        pf.asInstanceOf[PartialFunction[tb.tpd.Tree, tb.tpd.Tree]]
      ).asInstanceOf[Tree]
    }
  }

  implicit class TypedTreeOps(tree: tpd.Tree) {
    def pos: Pos = Pos.pos(tree)
    def tpe: Type = tpd.typeOf(tree)
    def wrap: Splice = TypedSplice(tree)
    def subst(from: List[Symbol], to: List[Symbol]): tpd.Tree = tpd.subst(tree)(from, to)
    def symbol: Option[Symbol] = tree.tpe.denot.map(_.symbol)

    def select(name: String): tpd.Tree = Select(tree, name)
    def appliedTo(args: tpd.Tree*): tpd.Tree = Apply(tree, args.toList)
    def appliedToTypes(args: tpd.Tree*): tpd.Tree = ApplyType(tree, args.toList)

    def traverse(pf: PartialFunction[tpd.Tree, Unit]): Unit =
      tpd.traverse(tree)(pf)

    def exists(pf: PartialFunction[tpd.Tree, Boolean]): Boolean =
      tpd.exists(tree)(pf)

    def transform(pf: PartialFunction[tpd.Tree, tpd.Tree]): tpd.Tree =
      tpd.transform(tree)(pf)
  }

  /**--------------------- ValDefs ---------------------------------*/
  def ValDef             = toolbox.ValDef.asInstanceOf[ValDefImpl]

  implicit class ValDefOps(tree: ValDef) {
    def mods: Mods = ValDef.mods(tree)
    def name: String = ValDef.name(tree)
    def tptOpt: Option[TypeTree] = ValDef.tptOpt(tree)
    def rhs: TermTree = ValDef.rhs(tree)
    def copy(rhs: TermTree = this.rhs): ValDef = ValDef.copyRhs(tree)(rhs)
  }

  implicit class ValDefTypedOps(tree: tpd.ValDef) {
    def symbol: Symbol = ValDef.symbol(tree)
    def name: String = ValDef.name(tree)
    def tptOpt: Option[TypeTree] = ValDef.tptOpt(tree)
    def rhs: TermTree = ValDef.rhs(tree)
    def copy(rhs: tpd.Tree): tpd.ValDef = ValDef.copyRhs(tree)(rhs)
  }

  object OfValDef {
    def unapply(tree: Tree): Option[ValDef] = ValDef.get(tree)
    def unapply(tree: tpd.Tree)(implicit c: Dummy): Option[tpd.ValDef] = ValDef.get(tree)(c)
  }

  /**--------------------- ValDecls ---------------------------------*/
  def ValDecl             = toolbox.ValDecl.asInstanceOf[ValDeclImpl]

  implicit class ValDeclOps(tree: ValDecl) {
    def mods: Mods = ValDecl.mods(tree)
    def name: String = ValDecl.name(tree)
    def tpt: TypeTree = ValDecl.tpt(tree)
  }

  object OfValDecl {
    def unapply(tree: Tree): Option[ValDecl] = ValDecl.get(tree)
  }

  /**--------------------- DefDefs ---------------------------------*/
  def DefDef              = toolbox.DefDef.asInstanceOf[DefDefImpl]

  implicit class DefDefOps(tree: DefDef) {
    def mods: Mods = DefDef.mods(tree)
    def tparams: List[TypeParam] = DefDef.tparams(tree)
    def paramss: List[List[Param]] = DefDef.paramss(tree)
    def name: String = DefDef.name(tree)
    def tptOpt: Option[TypeTree] = DefDef.tptOpt(tree)
    def rhs: TermTree = DefDef.rhs(tree)
    def copy(rhs: TermTree = this.rhs): DefDef = DefDef.copyRhs(tree)(rhs)
  }

  object OfDefDef {
    def unapply(tree: Tree): Option[DefDef] = DefDef.get(tree)
  }

  /**--------------------- DefDefs ---------------------------------*/
  def DefDecl              = toolbox.DefDecl.asInstanceOf[DefDeclImpl]

  implicit class DefDeclOps(tree: DefDecl) {
    def mods: Mods = DefDecl.mods(tree)
    def tparams: List[TypeParam] = DefDecl.tparams(tree)
    def paramss: List[List[Param]] = DefDecl.paramss(tree)
    def name: String = DefDecl.name(tree)
    def tpt: TypeTree = DefDecl.tpt(tree)
  }

  object OfDefDecl {
    def unapply(tree: Tree): Option[DefDecl] = DefDecl.get(tree)
  }

  /**--------------------- Param ---------------------------------*/
  def Param               = toolbox.Param.asInstanceOf[ParamImpl]

  implicit class ParamOps(tree: Param) {
    def mods: Mods = Param.mods(tree)
    def name: String = Param.name(tree)
    def tptOpt: Option[TypeTree] = Param.tptOpt(tree)
    def defaultOpt: Option[TermTree] = Param.defaultOpt(tree)
    def copy(mods: Mods = this.mods): Param = Param.copyMods(tree)(mods)
  }

  implicit class ParamTpdOps(tree: tpd.Param) {
    def symbol: Symbol = Param.symbol(tree)
    def name: String = Param.name(tree)
    def tpt: tpd.Tree = Param.tpt(tree)
  }

  /**--------------------- TypeParam ---------------------------------*/
  def TypeParam            = toolbox.TypeParam.asInstanceOf[TypeParamImpl]

  implicit class TypeParamOps(tree: TypeParam) {
    def mods: Mods = TypeParam.mods(tree)
    def name: String = TypeParam.name(tree)
    def tparams: List[TypeParam] = TypeParam.tparams(tree)
  }

  /**--------------------- Classes ---------------------------------*/
  def Class              = toolbox.Class.asInstanceOf[ClassImpl]

  implicit class ClassOps(tree: Class) {
    def mods: Mods = Class.mods(tree)
    def ctorMods: Mods = Class.ctorMods(tree)
    def name: String = Class.name(tree)
    def tparams: List[TypeParam] = Class.tparams(tree)
    def paramss: List[List[Param]] = Class.paramss(tree)
    def parents: List[InitCall] = Class.parents(tree)
    def selfOpt: Option[Self] = Class.selfOpt(tree)
    def stats: List[Tree] = Class.stats(tree)
    def copy(mods: Mods = Class.mods(tree),
             paramss: List[List[Param]] = Class.paramss(tree),
             stats: List[Tree] = Class.stats(tree)): Class = {
      val tree1 = Class.copyMods(tree)(mods)
      val tree2 = Class.copyParamss(tree1)(paramss)
      Class.copyStats(tree2)(stats)
    }
  }

  object OfClass {
    def unapply(tree: Tree): Option[Class] = Class.get(tree)
  }

  /**--------------------- Traits ---------------------------------*/
  def Trait            = toolbox.Trait.asInstanceOf[TraitImpl]

  implicit class TraitOps(tree: Trait) {
    def mods: Mods = Trait.mods(tree)
    def name: String = Trait.name(tree)
    def tparams: List[TypeParam] = Trait.tparams(tree)
    def paramss: List[List[Param]] = Trait.paramss(tree)
    def parents: List[InitCall] = Trait.parents(tree)
    def selfOpt: Option[Self] = Trait.selfOpt(tree)
    def stats: List[Tree] = Trait.stats(tree)
    def copy(stats: List[Tree] = Trait.stats(tree)): Trait
    = Trait.copyStats(tree)(stats)
  }

  object OfTrait {
    def unapply(tree: Tree): Option[Trait] = Trait.get(tree)
  }

  /**--------------------- Objects ---------------------------------*/
  def Object            = toolbox.Object.asInstanceOf[ObjectImpl]

  implicit class ObjectOps(tree: Object) {
    def mods: Mods = Object.mods(tree)
    def name: String = Object.name(tree)
    def parents: List[InitCall] = Object.parents(tree)
    def selfOpt: Option[Self] = Object.selfOpt(tree)
    def stats: List[Tree] = Object.stats(tree)
    def copy(stats: List[Tree] = Object.stats(tree)): Object
    = Object.copyStats(tree)(stats)
  }

  object OfObject {
    def unapply(tree: Tree): Option[Object] = Object.get(tree)
  }

  /**--------------------- helpers ---------------------------------*/
  def ApplySeq(fun: TermTree, argss: List[List[TermTree]]): TermTree =
    argss.foldLeft(fun) { (acc, args) => Apply(acc, args) }

  object ApplySeq {
    def unapply(call: TermTree): Option[(Tree, List[List[TermTree]])] = {
      def recur(acc: List[List[TermTree]], term: TermTree): (TermTree, List[List[TermTree]]) = term match {
        case api.Apply(fun, args) => recur(args +: acc, fun) // inner-most is in the front
        case fun => (fun, acc)
      }

      Some(recur(Nil, call))
    }

    def unapply(call: tpd.Tree)(implicit c: Dummy): Option[(tpd.Tree, List[List[tpd.Tree]])] =
      unapply(call.asInstanceOf[TermTree]).asInstanceOf[Option[(tpd.Tree, List[List[tpd.Tree]])]]
  }

  implicit def tpd2untpd(tree: tpd.Tree): Splice = TypedSplice(tree)
  // implicit def tpds2untpds(ts: List[tpd.Tree]): List[Tree] = ts.map(_.wrap)

  /**--------------------- Types ---------------------------------*/
  def Type           = toolbox.Type.asInstanceOf[TypeImpl]

  implicit class TypeOps(tp: Type) {
    def =:=(tp2: Type) = Type.=:=(tp, tp2)
    def <:<(tp2: Type) = Type.<:<(tp, tp2)
    def isCaseClass = Type.classSymbol(tp) match {
      case Some(cls) => Symbol.isCase(cls)
      case None      => false
    }
    def caseFields: List[Denotation] = Type.caseFields(tp)
    def fieldIn(name: String): Option[Denotation] = Type.fieldIn(tp, name)
    def fieldsIn: List[Denotation] = Type.fieldsIn(tp)
    def methodIn(name: String): List[Denotation] = Type.methodIn(tp, name)
    def methodsIn: List[Denotation] = Type.methodsIn(tp)
    def method(name: String): List[Denotation] = Type.method(tp, name)
    def methods: List[Denotation] = Type.methods(tp)
    def companion: Option[Type] = Type.companion(tp)
    def show: String = Type.show(tp)
    def widen: Type = Type.widen(tp)
    def denot: Option[Denotation] = Type.denot(tp)
    def symbol: Option[Symbol] = denot.map(_.symbol)
    def appliedTo(args: Type*): Type = Type.appliedTo(tp, args.toList)
    def toTree: tpd.Tree = Type.toTree(tp)
  }

  def ByNameType     = toolbox.ByNameType.asInstanceOf[ByNameTypeImpl]
  def MethodType     = toolbox.MethodType.asInstanceOf[MethodTypeImpl]

  implicit class MethodTypeOps(tp: MethodType) {
    def paramInfos: List[Type] = MethodType.paramInfos(tp)
    def instantiate(params: List[Type]): Type = MethodType.instantiate(tp)(params)
  }

  implicit class TermRefOps(tp: TermRef) {
    def symbol: Symbol    = denot.symbol
    def denot: Denotation = Type.denot(tp).get
  }

  /**--------------------- Symbols ---------------------------------*/
  def Symbol         = toolbox.Symbol.asInstanceOf[SymbolImpl]

  implicit class SymbolOps(sym: Symbol) {
    def name: String = Symbol.name(sym)
    def asSeenFrom(prefix: Type): Type = Symbol.asSeenFrom(sym, prefix)
    def isCase: Boolean = Symbol.isCase(sym)
    def isTrait: Boolean = Symbol.isTrait(sym)
    def isPrivate: Boolean = Symbol.isPrivate(sym)
    def isProtected: Boolean = Symbol.isProtected(sym)
    def isOverride: Boolean = Symbol.isOverride(sym)
    def isFinal: Boolean = Symbol.isFinal(sym)
    def isImplicit: Boolean = Symbol.isImplicit(sym)
    def isLazy: Boolean = Symbol.isLazy(sym)
    def isSealed: Boolean = Symbol.isSealed(sym)
    def isAbstract: Boolean = Symbol.isAbstract(sym)
    def isMutable: Boolean = Symbol.isMutable(sym)
  }

  /**--------------------- Denotations ---------------------------------*/
  def Denotation     = toolbox.Denotation.asInstanceOf[DenotationImpl]

  implicit class DenotationOps(denot: Denotation) {
    def name: String = Denotation.name(denot)
    def info: Type = Denotation.info(denot)
    def symbol: Symbol = Denotation.symbol(denot)
  }

  /**--------------------- misc ---------------------------------*/
  /** Placeholder of quasiquotes for type checking
   */
  implicit class QuasiquoteHelper(val sc: StringContext) {
    object q {
      def apply(args: Any*): Tree = ???
      def unapply(tree: Tree): Any = ???
    }

    object t {
      def apply(args: Any*): Tree = ???
      def unapply(tree: Tree): Any = ???
    }
  }
}


