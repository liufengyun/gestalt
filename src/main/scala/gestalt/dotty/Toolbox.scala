package scala.gestalt.dotty

import gestalt.core
import core.Location

import dotty.tools.dotc
import dotc.util.Positions.Position
import dotc.core.Decorators._
import dotc.core.Contexts
import dotc.core.NameKinds

class Toolbox(val enclosingPosition: Position)(implicit val ctx: Contexts.Context) extends core.Toolbox {
  type Context = Contexts.Context
  type Position = dotc.util.Positions.Position

  // members
  val untpd: Untpd = new gestalt.dotty.Untpd(this)
  val tpd: Tpd = new gestalt.dotty.Tpd(this)
  val types: Types = new gestalt.dotty.Types(this)
  val symbols: Symbols = new gestalt.dotty.Symbols(this)
  val denotations: Denotations = new gestalt.dotty.Denotations(this)

  def fresh(prefix: String = "$local"): String = NameKinds.UniqueName.fresh(prefix.toTermName).toString

  // diagnostics - the implementation takes the position from the tree
  def error(message: String, pos: Position): Unit =
    ctx.error(message, pos)

  def warn(message: String, pos: Position): Unit =
    ctx.warning(message, pos)

  /** stop macro transform - the implementation takes the position from the tree */
  def abort(message: String, pos: Position): Nothing = {
    ctx.error(message, pos)
    throw new Exception(message)
  }

  /** get the location where the def macro is used */
  def location: Location = Location(ctx.compilationUnit.source.file.name, enclosingPosition.line(), enclosingPosition.column())
}
