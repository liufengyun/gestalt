import scala.gestalt._

object JsonMacros {

  sealed trait JsValue
  final case class JsString(value: String) extends JsValue

  final case class JsObject(items: Seq[(String, JsValue)]) extends JsValue {
    def firstValue(key: String) = items.find(_._1 == key).map(_._2)
  }

  trait Format[A] {
    def toJson(o: A): JsValue
    def fromJson(json: JsValue): Option[A]
  }

  def format[T](): Format[T] = meta {
    import toolbox._
    val tpe: Type = T.tpe
    if (!tpe.isCaseClass) {
      error("Not a case class", T.pos)
      q"???"
    }
    else {
      val fields = tpe.caseFields
      val namesAndTypes = fields.map {
        f => f.name -> f.info
      }

      case class JsonItem(name: String, pairOut: TermTree, readOption: TermTree)

      val jsonItems = namesAndTypes.map {
        case (name, stringType) if stringType.show == "String" =>
          JsonItem(name,
            pairOut = q"${Lit(name)} -> JsonMacros.JsString(${Select(Ident("o"), name)})",
            readOption = q"json.firstValue(${Lit(name)}).collect{case JsString(value) => value}"
          )
        case (name, otherType) =>
          JsonItem(name,
            pairOut = q"${Lit(name)} -> JsonMacros.JsString(${Lit("No Idea")})", //TODO q"${Lit(name)} -> implicitly[Format[$otherType]].toJson(o.${Ident(name)})"
            readOption = q"None" // TODO
          )
      }
      q"""new JsonMacros.Format[$T]{
            def toJson(o: $T) = JsonMacros.JsObject(Seq(..${jsonItems.map(_.pairOut)}))
            def fromJson(json: JsonMacros.JsValue) = None
         }"""
    }
  }
}
