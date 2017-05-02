import scala.gestalt._

object JsonMacros {

  sealed trait JsValue
  final case class JsString(value: String) extends JsValue
  final case class JsObject(items: Seq[(String, JsValue)]) extends JsValue

  trait Format[A] {
    def toJson(o: A): JsValue
    def fromJson(json: JsValue): Option[A]
  }

  def format[T](): Format[T] = meta {
    import toolbox._
    val tpe: Type = T.tpe
    if (!tpe.isCaseClass) {
      toolbox.error("Not a case class", T)
      q"???"
    }
    else {
      val fields = tpe.caseFields
      q"""new Format[$T]{
            def toJson(o: $T) = JsObject(Nil)
            def fromJson(json: JsValue) = None
         }"""
    }
    q"null"
  }
}
