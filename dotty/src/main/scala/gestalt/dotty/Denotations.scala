package scala.gestalt.dotty

import gestalt.core
import dotty.tools.dotc
import dotc.core._

class Denotations(val toolbox: Toolbox) extends core.Denotations {
  import toolbox.types.Type
  import toolbox.symbols.Symbol

  type Denotation = Denotations.Denotation

  implicit val ctx: Contexts.Context = toolbox.ctx

  def name(denot: Denotation): String = denot.symbol.name.show

  def info(denot: Denotation): Type = denot.info.dealias

  def symbol(denot: Denotation): Symbol = denot.symbol
}
