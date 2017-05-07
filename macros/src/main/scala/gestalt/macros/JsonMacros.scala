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

      case class JsonItem(name: String, pairOut: TermTree, readOption: ValDef)

      val jsonItems = namesAndTypes.map {
        case (name, stringType) if stringType.show == "String" =>
          JsonItem(name,
            pairOut = q"${Lit(name)} -> JsString(${Select(Ident("o"), name)})",
            readOption = q"val $name = obj.firstValue(${Lit(name)}).collect{case JsString(value) => value}"
          )
        case (name, otherType) =>
          JsonItem(name,
            pairOut = q"${Lit(name)} -> JsString(${Lit("No Idea")})", //TODO q"${Lit(name)} -> implicitly[Format[$otherType]].toJson(o.${Ident(name)})"
            readOption = q"val $name = None" // TODO
          )
      }
      val allDefined = q"${jsonItems.map(i => q"${Ident(i.name)}.isDefined").reduceLeft((a, b) => q"$a && $b")}"
      val fromJson = q"""json match{
              case obj: JsObject =>
               {..${
                jsonItems.map(_.readOption) :+
                q"if($allDefined) Some(null) else None"
               }}
              case other => None
            }"""
      q"""{
          import JsonMacros._
          new Format[$T]{
            def toJson(o: $T) = JsObject(Seq(..${jsonItems.map(_.pairOut)}))
            def fromJson(json: JsValue) = $fromJson
         }
        }"""
    }
  }
}
