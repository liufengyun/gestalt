import scala.gestalt._
import quasiquotes._

trait Iso[T, U] {
  def to(t: T) : U
}

object Iso {
  implicit def materializeIso[T, U]: Iso[T, U] = meta {
    val fields = T.tpe.caseFields
    val fieldTps = fields.map(d => d.info)

    val tupleVal = {
        val args = fields.map(d => q"o.${d.name}")
        q"(..$args)"
    }

    val tupleTp = Type.typeRef("scala.Tuple" + fields.length).appliedTo(fieldTps: _*)

    q"""
      new Iso[$T, ${tupleTp.toTree}] {
          def to(o: $T) = $tupleVal
      }
    """
  }
}