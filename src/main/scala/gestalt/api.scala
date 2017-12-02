package scala.gestalt

import scala.gestalt.core

object api extends decos.Trees
              with decos.Types
              with decos.Symbols
              with decos.Denotations
{

  private val toolboxStore: ThreadLocal[core.Toolbox] = new ThreadLocal[core.Toolbox]

  @inline private[gestalt] def toolbox: core.Toolbox = toolboxStore.get

  def withToolbox[T](tb: core.Toolbox)(f: => T): T = {
    toolboxStore.set(tb)
    val res = f
    toolboxStore.remove()

    res
  }

  private[gestalt] implicit class XtensionBang[A](val a: A) extends AnyVal {
    def unary_![B]: B = a.asInstanceOf[B]
  }

  /*------------------------------- proxies -------------------------------------*/

  // An Unsafe capability is required to call the untyped Ident(name) and TypeIdent
  // in order to achieve hygiene
  type Unsafe >: Null <: AnyRef

  type Context
  type Position

  type Type = apis.Types.Type
  val Type: apis.Types.type = apis.Types

  type Symbol = apis.Symbols.Symbol
  val Symbol: apis.Symbols.type = apis.Symbols

  type Denotation = apis.Denotations.Denotation
  val Denotation: apis.Denotations.type = apis.Denotations

  val tpd: apis.Tpd.type = apis.Tpd
  val untpd: apis.Untpd.type = apis.Untpd

  /**------------------------------------------------*/
  def location: core.Location = toolbox.location

  def error(message: String, pos: Position) =
    toolbox.error(message, !pos)

  /** stop macro transform */
  def abort(message: String, pos: Position): Nothing =
    toolbox.abort(message, !pos)

  /** generate fresh unique name */
  def fresh(prefix: String = "$local"): String  = toolbox.fresh(prefix)

  /**--------------------- misc ---------------------------------*/
  /** Placeholder of quasiquotes for type checking
   */
  implicit class QuasiquoteHelper(val sc: StringContext) {
    object q {
      def apply(args: Any*): untpd.Tree = ???
      def unapply(tree: untpd.Tree): Any = ???
    }

    object t {
      def apply(args: Any*): untpd.Tree = ???
      def unapply(tree: untpd.Tree): Any = ???
    }
  }

  /** Avoid JVM same signature problem */
  implicit val dummy: core.Dummy = null
  implicit val dummy1: core.Dummy1 = null
  implicit val dummy2: core.Dummy2 = null
}
