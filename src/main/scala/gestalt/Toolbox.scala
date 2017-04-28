package scala.gestalt

case class Location(fileName: String, line: Int, column: Int)

trait Toolbox extends Trees with Symbols with Types {
  /** get the location where the def macro is used */
  def currentLocation: Location

  /** diagnostics - the implementation takes the position from the tree */
  def error(message: String, tree: Tree): Unit

  /** stop macro transform - the implementation takes the position from the tree */
  def abort(message: String, tree: Tree): Nothing

  /** generate fresh unique name */
  def fresh(prefix: String = "$local"): String
}



