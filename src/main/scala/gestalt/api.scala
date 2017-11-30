package scala.gestalt

import scala.gestalt.core._

// package object causes compilation errors
object api extends Toolbox {
  private val toolboxStore: ThreadLocal[Toolbox] = new ThreadLocal[Toolbox]

  @inline private def toolbox: Toolbox = toolboxStore.get

  def withToolbox[T](tb: Toolbox)(f: => T): T = {
    toolboxStore.set(tb)
    val res = f
    toolboxStore.remove()

    res
  }

  private implicit class XtensionBang[A](val a: A) extends AnyVal {
    def unary_![B]: B = a.asInstanceOf[B]
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

  def Ident(tp: TermRef)(implicit c: Dummy): tpd.Tree = Ident(Denotation.symbol(Type.denot(tp).get))

  /**------------------------------------------------*/
  // definitions
  def NewAnonymClass = !toolbox.NewAnonymClass
  def TypeDecl       = !toolbox.TypeDecl
  def TypeAlias      = !toolbox.TypeAlias
  def PatDef         = !toolbox.PatDef
  def InitCall       = !toolbox.InitCall
  def NewInstance    = !toolbox.NewInstance
  def SecondaryCtor  = !toolbox.SecondaryCtor
  def Self           = !toolbox.Self

  def ValDef          = !toolbox.ValDef
  def ValDecl         = !toolbox.ValDecl
  def DefDef          = !toolbox.DefDef
  def DefDecl         = !toolbox.DefDecl
  def Param           = !toolbox.Param
  def TypeParam       = !toolbox.TypeParam
  def Class           = !toolbox.Class
  def Trait           = !toolbox.Trait
  def Object          = !toolbox.Object

  // type trees
  def TypeIdent      = !toolbox.TypeIdent
  def TypeSelect     = !toolbox.TypeSelect
  def TypeSingleton  = !toolbox.TypeSingleton
  def TypeApply      = !toolbox.TypeApply
  def TypeInfix      = !toolbox.TypeInfix
  def TypeFunction   = !toolbox.TypeFunction
  def TypeTuple      = !toolbox.TypeTuple
  def TypeAnd        = !toolbox.TypeAnd
  def TypeOr         = !toolbox.TypeOr
  def TypeRefine     = !toolbox.TypeRefine
  def TypeBounds     = !toolbox.TypeBounds
  def TypeRepeated   = !toolbox.TypeRepeated
  def TypeByName     = !toolbox.TypeByName
  def TypeAnnotated  = !toolbox.TypeAnnotated

  // terms
  def Infix           = !toolbox.Infix
  def Prefix          = !toolbox.Prefix
  def Postfix         = !toolbox.Postfix
  def Throw           = !toolbox.Throw
  def Annotated       = !toolbox.Annotated
  def If              = !toolbox.If
  def Try             = !toolbox.Try
  def Function        = !toolbox.Function
  def While           = !toolbox.While
  def DoWhile         = !toolbox.DoWhile
  def For             = !toolbox.For
  def Named           = !toolbox.Named
  def Repeated        = !toolbox.Repeated
  def Lit             = !toolbox.Lit
  def Apply           = !toolbox.Apply
  def ApplyType       = !toolbox.ApplyType
  def Ident           = !toolbox.Ident
  def This            = !toolbox.This
  def Super           = !toolbox.Super
  def Select          = !toolbox.Select
  def Ascribe         = !toolbox.Ascribe
  def Assign          = !toolbox.Assign
  def Update          = !toolbox.Update
  def Return          = !toolbox.Return
  def Block           = !toolbox.Block
  def PartialFunction = !toolbox.PartialFunction
  def Match           = !toolbox.Match
  def Case            = !toolbox.Case
  def Tuple           = !toolbox.Tuple
  def Interpolate     = !toolbox.Interpolate
  def SeqLiteral      = !toolbox.SeqLiteral
  def TypedSplice     = !toolbox.TypedSplice


  def Import         = !toolbox.Import
  def Pat            = !toolbox.Pat

  /**--------------------- Positions ---------------------------------*/
  def Pos            = !toolbox.Pos

  /**--------------------- untyped TreeOps ---------------------------------*/
  def untpd          = !toolbox.untpd

  implicit class UntypedTreeOps(tree: Tree) {
    def pos: Pos = Pos.pos(tree)

    def show: String = untpd.show(tree)

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

  /**--------------------- typed TreeOps ---------------------------------*/
  val tpd: tpdImpl              =    null
  private def tpdOps: tpd.type  =    !toolbox.tpd

  implicit class TypedTreeOps(tree: tpd.Tree) {
    def pos: Pos = Pos.pos(tree)
    def tpe: Type = tpdOps.typeOf(tree)
    def show: String = tpdOps.show(tree)
    def wrap: Splice = TypedSplice(tree)
    def subst(from: List[Symbol], to: List[Symbol]): tpd.Tree = tpdOps.subst(tree)(from, to)
    def symbol: Option[Symbol] = tree.tpe.denot.map(_.symbol)
    def isDef: Boolean = tpdOps.isDef(tree)

    def select(name: String): tpd.Tree = Select(tree, name)
    def appliedTo(args: List[tpd.Tree]): tpd.Tree = Apply(tree, args.toList)
    def appliedToTypes(args: List[Type]): tpd.Tree = ApplyType(tree, args.toList)

    def traverse(pf: PartialFunction[tpd.Tree, Unit]): Unit =
      tpdOps.traverse(tree)(pf)

    def exists(pf: PartialFunction[tpd.Tree, Boolean]): Boolean =
      tpdOps.exists(tree)(pf)

    def transform(pf: PartialFunction[tpd.Tree, tpd.Tree]): tpd.Tree =
      tpdOps.transform(tree)(pf)
  }

  implicit class TpdDefTreeOps(tree: tpd.DefTree) {
    def symbol: Symbol = tpdOps.symbol(tree)
  }

  implicit class TpdRefTreeOps(tree: tpd.RefTree) {
    def symbol: Symbol = tpdOps.symbol(tree)
  }

  /**--------------------- helpers ---------------------------------*/
  def ApplySeq(fun: TermTree, argss: List[List[TermTree]]): TermTree =
    argss.foldLeft(fun) { (acc, args) => Apply(acc, args) }

  def ApplySeq(fun: tpd.Tree, argss: List[List[tpd.Tree]])(implicit c: Dummy): tpd.Tree =
    argss.foldLeft(fun) { (acc, args) => Apply(acc, args) }

  object ApplySeq {
    def unapply(call: tpd.Tree): Option[(tpd.Tree, List[List[tpd.Tree]])] = {
      def recur(acc: List[List[tpd.Tree]], term: tpd.Tree): (tpd.Tree, List[List[tpd.Tree]]) = term match {
        case api.Apply(fun, args) => recur(args +: acc, fun) // inner-most is in the front
        case fun => (fun, acc)
      }

      Some(recur(Nil, call))
    }
  }

  implicit def tpd2untpd(tree: tpd.Tree): Splice = TypedSplice(tree)
  implicit def tpd2untpdList(trees: List[tpd.Tree]): List[TermTree] = trees.map(TypedSplice.apply)
  implicit def tpd2untpdListList(treess: List[List[tpd.Tree]]): List[List[TermTree]] = treess.map(_.map(TypedSplice.apply))

  /**--------------------- Types ---------------------------------*/
  def Type                     = !toolbox.Type
  def ByNameType               = !toolbox.ByNameType
  def AppliedType              = !toolbox.AppliedType
  def MethodType               = !toolbox.MethodType

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
    def termSymbol: Option[Symbol] = denot.map(_.symbol)
    def classSymbol: Option[Symbol] = Type.classSymbol(tp)
    def appliedTo(args: Type*): Type = AppliedType(tp, args.toList)
    def toTree: tpd.Tree = Type.toTree(tp)
  }

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
    def termRef: TermRef = Symbol.termRef(sym)
    def typeRef: TypeRef = Symbol.typeRef(sym)
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


