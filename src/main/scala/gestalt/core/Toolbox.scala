package scala.gestalt.core

case class Location(fileName: String, line: Int, column: Int)

trait Toolbox extends Trees with Types with Denotations with Symbols with TypeTags {
  trait Dummy
  implicit val dummy: Dummy = null

  trait Dummy1
  implicit val dummy1: Dummy1 = null

  // An Unsafe capability is required to call the untyped Ident(name) and TypeIdent
  // in order to achieve hygiene
  type Unsafe

  /** get the location where the def macro is used */
  def location: Location

  /** diagnostics */
  def error(message: String, pos: Pos): Unit

  /** stop macro transform */
  def abort(message: String, pos: Pos): Nothing

  /** generate fresh unique name */
  def fresh(prefix: String = "$local"): String
}
