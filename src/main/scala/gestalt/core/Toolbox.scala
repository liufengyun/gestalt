package scala.gestalt.core

// for avoid same signature error on JVM
trait Dummy
trait Dummy1
trait Dummy2

case class Location(fileName: String, line: Int, column: Int)

trait Toolbox {
  type Context
  type Position

  // An Unsafe capability is required to call the untyped Ident(name) and TypeIdent
  // in order to achieve hygiene
  type Unsafe >: Null <: AnyRef


  // members
  val untpd: Untpd
  val tpd: Tpd
  val types: Types
  val symbols: Symbols
  val denotations: Denotations

  // type proxy
  type Type       =   types.Type
  type Symbol     =   symbols.Symbol
  type Denotation =   denotations.Denotation

  /** get the location where the def macro is used */
  def location: Location

  /** produce an error */
  def error(message: String, pos: Position): Unit

  /** stop macro transform */
  def abort(message: String, pos: Position): Nothing

  /** produce a warning */
  def warn(message: String, pos: Position): Unit

  /** generate fresh unique name */
  def fresh(prefix: String = "$local"): String
}
