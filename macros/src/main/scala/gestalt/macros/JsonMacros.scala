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

  /**
    * Creates a {{{Format}}} for the case class.
    * Limitations:
    * 1. does not handle cyclic references, i.e. {{{case class A(field: A, ...)}}}
    * 2. does not handle optional arguments
    * @tparam T type of the case class
    * @return new anonymous Format class
    */
  def format[T](): Format[T] = meta {
    import toolbox._
    val tpe: Type = T.tpe
    if (!tpe.isCaseClass) {
      error("Not a case class", T.pos)
      q"???"
    } else {
      val fields = tpe.caseFields
      val namesAndTypes = fields.map {
        f => f.name -> f.info
      }

      case class JsonItem(name: String, pairOut: TermTree, readOption: ValDef, implicitFormat: Option[ValDef])
      val jsonItems: Seq[JsonItem] = namesAndTypes.map {
        case (name, stringType) if stringType =:= Type.typeRef("java.lang.String") =>
          JsonItem(name,
            pairOut = Tuple(Seq(Lit(name), q"JsString(${Select(Ident("o"), name)})")),
            readOption = q"val $name = obj.firstValue(${Lit(name)}).collect{case JsString(value) => value}",
            implicitFormat = None
          )
        case (name, otherType) =>
          val implFormaterName = name + "_formatter"
          JsonItem(name,
            pairOut = Tuple(Seq(Lit(name), q"${Ident(implFormaterName)}.toJson(${Select(Ident("o"), name)})")),
            readOption = q"val $name = obj.firstValue(${Lit(name)}).flatMap(x =>${Ident(implFormaterName)}.fromJson(x))",
            implicitFormat = Some(q"val $implFormaterName=implicitly[Format[${otherType.toTree}]]")
          )
      }
      val allDefined = q"${jsonItems.map(i => q"${Ident(i.name)}.isDefined").reduceLeft((a, b) => q"$a && $b")}"
      val construction = NewInstance(T, Nil, Seq(jsonItems.map(i => q"${Ident(i.name)}.get")))
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
