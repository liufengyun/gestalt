package scala

package object gestalt extends decos.Trees
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

  type WeakTypeTag = api.WeakTypeTag

  type Type = api.Types.Type
  val Type: api.Types.type = api.Types

  type Symbol = api.Symbols.Symbol
  val Symbol: api.Symbols.type = api.Symbols

  type Denotation = api.Denotations.Denotation
  val Denotation: api.Denotations.type = api.Denotations

  val tpd: api.Tpd.type = api.Tpd
  val untpd: api.Untpd.type = api.Untpd

  /**------------------------------------------------*/
  def location: core.Location = toolbox.location

  def error(message: String, pos: Position): Unit =
    toolbox.error(message, !pos)

  /** stop macro transform */
  def abort(message: String, pos: Position): Nothing =
    toolbox.abort(message, !pos)

  def warn(message: String, pos: Position): Unit =
    toolbox.warn(message, !pos)

  /** generate fresh unique name */
  def fresh(prefix: String = "$local"): String  = toolbox.fresh(prefix)

  /**--------------------- misc ---------------------------------*/

  /** Avoid JVM same signature problem */
  implicit val dummy: core.Dummy = null
  implicit val dummy1: core.Dummy1 = null
  implicit val dummy2: core.Dummy2 = null
}
