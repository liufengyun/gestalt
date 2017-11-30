package scala.gestalt

import scala.gestalt.core._

// package object causes compilation errors
object api extends Toolbox with helpers.Trees
                           with helpers.Types
                           with helpers.Symbols
                           with helpers.Denotations
{
  private val toolboxStore: ThreadLocal[Toolbox] = new ThreadLocal[Toolbox]

  @inline private[gestalt] def toolbox: Toolbox = toolboxStore.get

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

  /**
   *  For hygiene, by default only fully-qualified names are allowed, unless
   *  `scala.gestalt.options.unsafe` is imported.
   */
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
  /** generate fresh unique name */
  def fresh(prefix: String = "$local"): String  = toolbox.fresh(prefix)

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

  def ValDef         = !toolbox.ValDef
  def ValDecl        = !toolbox.ValDecl
  def DefDef         = !toolbox.DefDef
  def DefDecl        = !toolbox.DefDecl
  def Param          = !toolbox.Param
  def TypeParam      = !toolbox.TypeParam
  def Class          = !toolbox.Class
  def Trait          = !toolbox.Trait
  def Object         = !toolbox.Object

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


  def Import          = !toolbox.Import
  def Pat             = !toolbox.Pat

  /**--------------------- Positions ---------------------------------*/
  def Pos             = !toolbox.Pos

  /**--------------------- untyped TreeOps ---------------------------------*/
  def untpd           = !toolbox.untpd

  /**--------------------- typed TreeOps ---------------------------------*/
  val tpd: tpdImpl    =    null

  /**--------------------- Types ---------------------------------*/
  def Type            = !toolbox.Type
  def ByNameType      = !toolbox.ByNameType
  def AppliedType     = !toolbox.AppliedType
  def MethodType      = !toolbox.MethodType

  /**--------------------- Symbols ---------------------------------*/
  def Symbol          = toolbox.Symbol.asInstanceOf[SymbolImpl]


  /**--------------------- Denotations ---------------------------------*/
  def Denotation      = toolbox.Denotation.asInstanceOf[DenotationImpl]

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


