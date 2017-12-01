package scala.gestalt
package apis

object Denotations {
  import api.{ toolbox => impl, XtensionBang, Symbol, Type }

  type Denotation

  def name(denot: Denotation): String = impl.denotations.name(!denot)

  def info(denot: Denotation): Type = !impl.denotations.info(!denot)

  def symbol(denot: Denotation): Symbol = !impl.denotations.symbol(!denot)
}
