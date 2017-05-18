package scala.gestalt

import gestalt.core._

// package object causes compilation errors
object api extends Toolbox { pkg =>
  private val toolbox: ThreadLocal[Toolbox] = new ThreadLocal[Toolbox]

  type Location = core.Location

  def withToolbox[T](tb: Toolbox)(f: => T): T = {
    toolbox.set(tb)
    val res = f
    toolbox.remove()

    res
  }

  def emptyMods: Mods = toolbox.get.emptyMods.asInstanceOf[Mods]

  // diagnostics
  def location: Location = toolbox.get.location

  def error(message: String, pos: Pos) = {
    val tb = toolbox.get
    tb.error(message, pos.asInstanceOf[tb.Pos])
  }

  /** stop macro transform */
  def abort(message: String, pos: Pos): Nothing = {
    val tb = toolbox.get
    tb.abort(message, pos.asInstanceOf[tb.Pos])
  }

  /** generate fresh unique name */
  def fresh(prefix: String = "$local"): String  = toolbox.get.fresh(prefix)

  /**------------------------------------------------*/
  // definitions
  def NewAnonymClass = toolbox.get.NewAnonymClass.asInstanceOf[NewAnonymClassImpl]
  def TypeDecl       = toolbox.get.TypeDecl.asInstanceOf[TypeDeclImpl]
  def TypeAlias      = toolbox.get.TypeAlias.asInstanceOf[TypeAliasImpl]
  def PatDef         = toolbox.get.PatDef.asInstanceOf[PatDefImpl]
  def SeqDef         = toolbox.get.SeqDef.asInstanceOf[SeqDefImpl]
  def SeqDecl        = toolbox.get.SeqDecl.asInstanceOf[SeqDeclImpl]
  def InitCall       = toolbox.get.InitCall.asInstanceOf[InitCallImpl]
  def NewInstance    = toolbox.get.NewInstance.asInstanceOf[NewInstanceImpl]
  def SecondaryCtor  = toolbox.get.SecondaryCtor.asInstanceOf[SecondaryCtorImpl]
  def Self           = toolbox.get.Self.asInstanceOf[SelfImpl]

  // type trees
  def TypeIdent      = toolbox.get.TypeIdent.asInstanceOf[TypeIdentImpl]
  def TypeSelect     = toolbox.get.TypeSelect.asInstanceOf[TypeSelectImpl]
  def TypeSingleton  = toolbox.get.TypeSingleton.asInstanceOf[TypeSingletonImpl]
  def TypeApply      = toolbox.get.TypeApply.asInstanceOf[TypeApplyImpl]
  def TypeInfix      = toolbox.get.TypeInfix.asInstanceOf[TypeInfixImpl]
  def TypeFunction   = toolbox.get.TypeFunction.asInstanceOf[TypeFunctionImpl]
  def TypeTuple      = toolbox.get.TypeTuple.asInstanceOf[TypeTupleImpl]
  def TypeAnd        = toolbox.get.TypeAnd.asInstanceOf[TypeAndImpl]
  def TypeOr         = toolbox.get.TypeOr.asInstanceOf[TypeOrImpl]
  def TypeRefine     = toolbox.get.TypeRefine.asInstanceOf[TypeRefineImpl]
  def TypeBounds     = toolbox.get.TypeBounds.asInstanceOf[TypeBoundsImpl]
  def TypeRepeated   = toolbox.get.TypeRepeated.asInstanceOf[TypeRepeatedImpl]
  def TypeByName     = toolbox.get.TypeByName.asInstanceOf[TypeByNameImpl]
  def TypeAnnotated  = toolbox.get.TypeAnnotated.asInstanceOf[TypeAnnotatedImpl]

  // terms
  def Infix           = toolbox.get.Infix.asInstanceOf[InfixImpl]
  def Prefix          = toolbox.get.Prefix.asInstanceOf[PrefixImpl]
  def Postfix         = toolbox.get.Postfix.asInstanceOf[PostfixImpl]
  def Throw           = toolbox.get.Throw.asInstanceOf[ThrowImpl]
  def Annotated       = toolbox.get.Annotated.asInstanceOf[AnnotatedImpl]
  def If              = toolbox.get.If.asInstanceOf[IfImpl]
  def Try             = toolbox.get.Try.asInstanceOf[TryImpl]
  def Function        = toolbox.get.Function.asInstanceOf[FunctionImpl]
  def While           = toolbox.get.While.asInstanceOf[WhileImpl]
  def DoWhile         = toolbox.get.DoWhile.asInstanceOf[DoWhileImpl]
  def For             = toolbox.get.For.asInstanceOf[ForImpl]
  def Named           = toolbox.get.Named.asInstanceOf[NamedImpl]
  def Repeated        = toolbox.get.Repeated.asInstanceOf[RepeatedImpl]
  def Lit             = toolbox.get.Lit.asInstanceOf[LitImpl]
  def Apply           = toolbox.get.Apply.asInstanceOf[ApplyImpl]
  def ApplyType       = toolbox.get.ApplyType.asInstanceOf[ApplyTypeImpl]
  def Ident           = toolbox.get.Ident.asInstanceOf[IdentImpl]
  def This            = toolbox.get.This.asInstanceOf[ThisImpl]
  def Super           = toolbox.get.Super.asInstanceOf[SuperImpl]
  def Select          = toolbox.get.Select.asInstanceOf[SelectImpl]
  def Ascribe         = toolbox.get.Ascribe.asInstanceOf[AscribeImpl]
  def Assign          = toolbox.get.Assign.asInstanceOf[AssignImpl]
  def Update          = toolbox.get.Update.asInstanceOf[UpdateImpl]
  def Return          = toolbox.get.Return.asInstanceOf[ReturnImpl]
  def Block           = toolbox.get.Block.asInstanceOf[BlockImpl]
  def PartialFunction = toolbox.get.PartialFunction.asInstanceOf[PartialFunctionImpl]
  def Match           = toolbox.get.Match.asInstanceOf[MatchImpl]
  def Case            = toolbox.get.Case.asInstanceOf[CaseImpl]
  def Tuple           = toolbox.get.Tuple.asInstanceOf[TupleImpl]
  def Interpolate     = toolbox.get.Interpolate.asInstanceOf[InterpolateImpl]
  def SeqLiteral      = toolbox.get.SeqLiteral.asInstanceOf[SeqLiteralImpl]
  def TypedSplice     = toolbox.get.TypedSplice.asInstanceOf[TypedSpliceImpl]


  def Import         = toolbox.get.Import.asInstanceOf[ImportImpl]
  def Pat            = toolbox.get.Pat.asInstanceOf[PatImpl]

  /**--------------------- Positions ---------------------------------*/
  def Pos            = toolbox.get.Pos.asInstanceOf[PosImpl]

  /**--------------------- TreeOps ---------------------------------*/
  def untpd          = toolbox.get.untpd.asInstanceOf[untpdImpl]

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
    def select(name: String): TermTree = Select(tree, name)
    def appliedTo(args: TermTree*): TermTree = Apply(tree, args.toList)
  }

  object tpd extends tpdImpl {
    def typeOf(tree: Tree): Type = {
      val tb = toolbox.get
      tb.tpd.typeOf(tree.asInstanceOf[tb.tpd.Tree]).asInstanceOf[Type]
    }

    def subst(tree: Tree)(from: List[Symbol], to: List[Symbol]): Tree = {
      val tb = toolbox.get
      tb.tpd.subst(tree.asInstanceOf[tb.tpd.Tree])(
        from.asInstanceOf[List[tb.Symbol]],
        to.asInstanceOf[List[tb.Symbol]]
      ).asInstanceOf[Tree]
    }

    def traverse(tree: Tree)(pf: PartialFunction[Tree, Unit]): Unit = {
      val tb = toolbox.get
      tb.tpd.traverse(tree.asInstanceOf[tb.tpd.Tree])(
        pf.asInstanceOf[PartialFunction[tb.tpd.Tree, Unit]]
      )
    }

    def exists(tree: Tree)(pf: PartialFunction[Tree, Boolean]): Boolean = {
      val tb = toolbox.get
      tb.tpd.exists(tree.asInstanceOf[tb.tpd.Tree])(
        pf.asInstanceOf[PartialFunction[tb.tpd.Tree, Boolean]]
      )
    }

    def transform(tree: Tree)(pf: PartialFunction[Tree, Tree]): Tree = {
      val tb = toolbox.get
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
    def appliedTo(args: tpd.Tree*): tpd.Tree = Apply(tree, args)
    def appliedToTypes(args: tpd.Tree*): tpd.Tree = ApplyType(tree, args)

    def traverse(pf: PartialFunction[tpd.Tree, Unit]): Unit =
      tpd.traverse(tree)(pf)

    def exists(pf: PartialFunction[tpd.Tree, Boolean]): Boolean =
      tpd.exists(tree)(pf)

    def transform(pf: PartialFunction[tpd.Tree, tpd.Tree]): tpd.Tree =
      tpd.transform(tree)(pf)
  }

  /**--------------------- ValDefs ---------------------------------*/
  def ValDef             = toolbox.get.ValDef.asInstanceOf[ValDefImpl]

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
    def unapply(tree: tpd.Tree)(implicit c: Cap): Option[tpd.ValDef] = ValDef.get(tree)(c)
  }

  /**--------------------- ValDecls ---------------------------------*/
  def ValDecl             = toolbox.get.ValDecl.asInstanceOf[ValDeclImpl]

  implicit class ValDeclOps(tree: ValDecl) {
    def mods: Mods = ValDecl.mods(tree)
    def name: String = ValDecl.name(tree)
    def tpt: TypeTree = ValDecl.tpt(tree)
  }

  object OfValDecl {
    def unapply(tree: Tree): Option[ValDecl] = ValDecl.get(tree)
  }

  /**--------------------- DefDefs ---------------------------------*/
  def DefDef              = toolbox.get.DefDef.asInstanceOf[DefDefImpl]

  implicit class DefDefOps(tree: DefDef) {
    def mods: Mods = DefDef.mods(tree)
    def tparams: Seq[TypeParam] = DefDef.tparams(tree)
    def paramss: Seq[Seq[Param]] = DefDef.paramss(tree)
    def name: String = DefDef.name(tree)
    def tptOpt: Option[TypeTree] = DefDef.tptOpt(tree)
    def rhs: TermTree = DefDef.rhs(tree)
    def copy(rhs: TermTree = this.rhs): DefDef = DefDef.copyRhs(tree)(rhs)
  }

  object OfDefDef {
    def unapply(tree: Tree): Option[DefDef] = DefDef.get(tree)
  }

  /**--------------------- DefDefs ---------------------------------*/
  def DefDecl              = toolbox.get.DefDecl.asInstanceOf[DefDeclImpl]

  implicit class DefDeclOps(tree: DefDecl) {
    def mods: Mods = DefDecl.mods(tree)
    def tparams: Seq[TypeParam] = DefDecl.tparams(tree)
    def paramss: Seq[Seq[Param]] = DefDecl.paramss(tree)
    def name: String = DefDecl.name(tree)
    def tpt: TypeTree = DefDecl.tpt(tree)
  }

  object OfDefDecl {
    def unapply(tree: Tree): Option[DefDecl] = DefDecl.get(tree)
  }

  /**--------------------- Param ---------------------------------*/
  def Param               = toolbox.get.Param.asInstanceOf[ParamImpl]

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
  def TypeParam            = toolbox.get.TypeParam.asInstanceOf[TypeParamImpl]

  implicit class TypeParamOps(tree: TypeParam) {
    def mods: Mods = TypeParam.mods(tree)
    def name: String = TypeParam.name(tree)
    def tparams: Seq[TypeParam] = TypeParam.tparams(tree)
  }

  /**--------------------- Classes ---------------------------------*/
  def Class              = toolbox.get.Class.asInstanceOf[ClassImpl]

  implicit class ClassOps(tree: Class) {
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

  object OfClass {
    def unapply(tree: Tree): Option[Class] = Class.get(tree)
  }

  /**--------------------- Traits ---------------------------------*/
  def Trait            = toolbox.get.Trait.asInstanceOf[TraitImpl]

  implicit class TraitOps(tree: Trait) {
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

  object OfTrait {
    def unapply(tree: Tree): Option[Trait] = Trait.get(tree)
  }

  /**--------------------- Objects ---------------------------------*/
  def Object            = toolbox.get.Object.asInstanceOf[ObjectImpl]

  implicit class ObjectOps(tree: Object) {
    def mods: Mods = Object.mods(tree)
    def name: String = Object.name(tree)
    def parents: Seq[InitCall] = Object.parents(tree)
    def selfOpt: Option[Self] = Object.selfOpt(tree)
    def stats: Seq[Tree] = Object.stats(tree)
    def copy(stats: Seq[Tree] = Object.stats(tree)): Object
    = Object.copyStats(tree)(stats)
  }

  object OfObject {
    def unapply(tree: Tree): Option[Object] = Object.get(tree)
  }

  /**--------------------- helpers ---------------------------------*/
  def ApplySeq(fun: TermTree, argss: Seq[Seq[TermTree]]): TermTree =
    argss.foldLeft(fun) { (acc, args) => Apply(acc, args) }

  object ApplySeq {
    def unapply(call: TermTree): Option[(Tree, Seq[Seq[TermTree]])] = {
      def recur(acc: Seq[Seq[TermTree]], term: TermTree): (TermTree, Seq[Seq[TermTree]]) = term match {
        case pkg.Apply(fun, args) => recur(args +: acc, fun) // inner-most is in the front
        case fun => (fun, acc)
      }

      Some(recur(Nil, call))
    }

    def unapply(call: tpd.Tree)(implicit c: Cap): Option[(tpd.Tree, Seq[Seq[tpd.Tree]])] =
      unapply(call.asInstanceOf[TermTree]).asInstanceOf[Option[(tpd.Tree, Seq[Seq[tpd.Tree]])]]
  }

  implicit def tpd2untpd(tree: tpd.Tree): Splice = TypedSplice(tree)

  /**--------------------- Types ---------------------------------*/
  def Type           = toolbox.get.Type.asInstanceOf[TypeImpl]

  implicit class TypeOps(tp: Type) {
    def =:=(tp2: Type) = Type.=:=(tp, tp2)
    def <:<(tp2: Type) = Type.<:<(tp, tp2)
    def isCaseClass = Type.isCaseClass(tp)
    def caseFields: Seq[Denotation] = Type.caseFields(tp)
    def fieldIn(name: String): Option[Denotation] = Type.fieldIn(tp, name)
    def fieldsIn: Seq[Denotation] = Type.fieldsIn(tp)
    def methodIn(name: String): Seq[Denotation] = Type.methodIn(tp, name)
    def methodsIn: Seq[Denotation] = Type.methodsIn(tp)
    def method(name: String): Seq[Denotation] = Type.method(tp, name)
    def methods: Seq[Denotation] = Type.methods(tp)
    def companion: Option[Type] = Type.companion(tp)
    def show: String = Type.show(tp)
    def widen: Type = Type.widen(tp)
    def denot: Option[Denotation] = Type.denot(tp)
    def symbol: Option[Symbol] = denot.map(_.symbol)
    def appliedTo(args: Type*): Type = Type.appliedTo(tp, args)
    def toTree: tpd.Tree = Type.toTree(tp)
  }

  def ByNameType     = toolbox.get.ByNameType.asInstanceOf[ByNameTypeImpl]
  def MethodType     = toolbox.get.MethodType.asInstanceOf[MethodTypeImpl]

  implicit class MethodTypeOps(tp: MethodType) {
    def paramInfos: Seq[Type] = MethodType.paramInfos(tp)
    def instantiate(params: Seq[Type]): Type = MethodType.instantiate(tp)(params)
  }

  /**--------------------- Symbols ---------------------------------*/
  def Symbol         = toolbox.get.Symbol.asInstanceOf[SymbolImpl]


  implicit class SymbolOps(sym: Symbol) {
    def name: String = Symbol.name(sym)
    def asSeenFrom(prefix: Type): Type = Symbol.asSeenFrom(sym, prefix)
  }

  /**--------------------- Denotations ---------------------------------*/
  def Denotation     = toolbox.get.Denotation.asInstanceOf[DenotationImpl]

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
