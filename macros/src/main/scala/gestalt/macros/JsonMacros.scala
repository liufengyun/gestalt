import scala.gestalt._
import untpd.{ Ident, Tuple, Lit, NewInstance }

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

  /**
    * Creates a {{{Format}}} for the case class.
    * Limitations:
    * 1. does not handle cyclic references, i.e. {{{case class A(field: A, ...)}}}
    * 2. does not handle optional arguments
    * @tparam T type of the case class
    * @return new anonymous Format class
    */
  def format[T](): Format[T] = meta {
    import scala.gestalt.options.unsafe
    val tpe: Type = T.tpe
    if (!tpe.isCaseClass) {
      error("Not a case class", T.pos)
      q"???"
    } else {
      val fields = tpe.caseFields
      val fieldsWithTypes = fields.map {
        f => f-> f.info
      }

      case class JsonItem(name: String, value: untpd.Ident, pairOut: untpd.TermTree, readOption: untpd.ValDef, implicitFormat: Option[untpd.ValDef])
      val jsonItems: List[JsonItem] = fieldsWithTypes.map {
        case (field, stringType) if stringType =:= Type.typeRef("java.lang.String") =>
          val name = field.name
          JsonItem(name,
            pairOut = Tuple(List(Lit(name), q"JsString(o.$name)")),
            value = Ident(name),
            readOption = q"val $name = obj.firstValue(${Lit(name)}).collect{case JsString(value) => value}",
            implicitFormat = None
          )
        case (field, otherType) =>
          val name = field.name
          val implFormaterName = name + "_formatter"
          val formatterIdent = Ident(implFormaterName)
          JsonItem(name,
            pairOut = Tuple(List(Lit(name), q"$formatterIdent.toJson(o.$name)")),
            value = Ident(name),
            readOption = q"val $name = obj.firstValue(${Lit(name)}).flatMap(x =>$formatterIdent.fromJson(x))",
            implicitFormat = Some(q"val $implFormaterName=implicitly[Format[${otherType.toTree}]]")
          )
      }
      val allDefined = q"${jsonItems.map(i => q"${i.value}.isDefined").reduceLeft((a, b) => q"$a && $b")}"
      val construction = NewInstance(T, List(jsonItems.map(i => q"${i.value}.get")))
      val fromJson =
        q"""json match{
              case obj: JsObject =>
               {..${
          jsonItems.map(_.readOption) :+
            q"if($allDefined) Some($construction) else None"
        }}
              case other => None
            }"""
      q"""
          import JsonMacros._
          new Format[$T]{..${
        jsonItems.flatMap(_.implicitFormat).toList :+
          q"def toJson(o: $T) = JsObject(Seq(..${jsonItems.map(_.pairOut)}))" :+
          q"def fromJson(json: JsValue) = $fromJson"
      }}
        """
    }
  }
}
