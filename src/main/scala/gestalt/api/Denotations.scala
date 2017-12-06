package scala.gestalt.api

import scala.gestalt.{ toolbox => impl, _ }

object Denotations {
  type Denotation >: Null <: AnyRef

  def name(denot: Denotation): String = impl.denotations.name(!denot)

  def info(denot: Denotation): Type = !impl.denotations.info(!denot)

  def symbol(denot: Denotation): Symbol = !impl.denotations.symbol(!denot)
}
