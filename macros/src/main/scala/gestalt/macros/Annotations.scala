import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq

import scala.gestalt.api._
import scala.gestalt.options.unsafe

class main extends StaticAnnotation {
  def apply(defn: Any): Any = meta {
    val q"$mods object $name { ..$stats }" = defn
    val main = q"""
      def stub(args: Any): Any = { ..$stats }
    """
    q"$mods object $name { $main }"
  }
}

class addFields extends StaticAnnotation {
  def apply(defn: Any): Any = meta {
    val q"$mods object $name { ..${stats:Seq[Tree]} }" = defn
    val additional: Seq[Tree] = Seq(
      q"private def a = 1",
      q"private[this] def b = 2",
      q"private[pack] def c = 3",

      q"protected def a1 = 1",
      q"protected[this] def b1 = 2",
      q"protected[pack] def c1 = 3",

      q"def all = List(a,b,c, a1,b1,c1)"
    )
    q"$mods object $name { ..${stats ++ additional} }"
  }
}

class replace extends StaticAnnotation {
  def apply(defn: Any): Any = meta {
    q"object UnrelatedObject{ def aPrimeNumber = 29 }"
  }
}


/*
class data extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"class $name(..$params)" =>
        val params1 = params.map(_.copy(mods = List(mod"valparam")))
        val class1 = q"class $name(..$params1)"

        val applyParams = params.map(_.copy(mods = Nil))
        val applyArgs = params.map(p => Term.Name(p.name.syntax))
        val apply = q"def apply(..$applyParams) = new ${Ctor.Name(name.syntax)}(..$applyArgs)"
        val object1 = q"object ${Term.Name(name.syntax)} { $apply }"

        q"$class1; $object1"
      case _ =>
        abort("@data can only annotate classes")
    }
  }
}

class xsd(fileName: String) extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"object $name" =>
        val a = 3
        q"val x = $a" // test lifting inside meta block

        val q"new xsd(${fileName: String})" = this
        val schema = loadSchema(fileName).map(xsdComplexType => {
          val name = Type.Name(xsdComplexType.name)
          val fields = xsdComplexType.fields.map(xsdField => {
            val fieldName = Term.Name(xsdField.name)
            val fieldType = xsdField.tpe.parse[Type].get
            param"$fieldName: ${Some(fieldType)}"
          })
          q"class $name(..$fields)"
        })
        q"object $name { ..$schema }"
      case _ =>
        abort("@xsd can only annotate objects")
    }
  }
}

case class XsdComplexType(name: String, fields: List[XsdField])
case class XsdField(name: String, tpe: String)

object loadSchema {
  def apply(fileName: String): List[XsdComplexType] = {
    val xmlSchema = scala.xml.XML.load(new java.io.FileInputStream(fileName))
    (xmlSchema \ "element").toList.map(xmlElement => {
      val schemaName = xmlElement \@ "name"
      val xmlFields = xmlElement \ "complexType" \ "sequence" \ "element"
      val schemaFields = xmlFields.toList.map(xmlField => {
        val schemaName = xmlField \@ "name"
        val schemaType = (xmlField \@ "type") match {
          case "xs:string" => "_root_.java.lang.String"
          case other => sys.error("unsupported field type: " + other)
        }
        XsdField(schemaName, schemaType)
      })
      XsdComplexType(schemaName, schemaFields)
    })
  }
}

object dynamic {
  def name(i: Int) = s"dynamicParam$i"
  def generate(tps: Seq[Type]): Seq[Term.Param] =
    for {
      (tp, index) <- tps.zipWithIndex
      pname = Term.Name(name(index))
    } yield param"$pname : $tp"

  def flatten(tp: Type): Seq[Type] = tp match {
    case Type.And(tp1, tp2) => flatten(tp1) ++ flatten(tp2)
    case _ => List(tp)
  }
}

class dynamic[T] extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"..$mods def $name[..$tparams](...$params): $tpe" =>
        val dtparams = this match {
          case q"new $_[$tparam]"  => dynamic.flatten(tparam)
        }
        val params2 = params :+ dynamic.generate(dtparams)
        q"..$mods def $name[..$tparams](...$params2): $tpe"
      case q"..$mods def $name[..$tparams](...$params): $tpe = $body" =>
        val dtparams = this match {
          case q"new $_[$tparam]"  => dynamic.flatten(tparam)
        }
        val params2 = params :+ dynamic.generate(dtparams)
        q"..$mods def $name[..$tparams](...$params2): $tpe = $body"
      case _ =>
        abort("@dynamic can only annotate methods")
    }
  }
}

class cache extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"..$mods def $name[..$tparams](...$params): $tpe = $body" =>
        if (tpe.isEmpty)
          abort("@cache can only annotate method with explicit return type")

        val termName = Term.Name("_" + name.toString)
        val patName = Pat.Var.Term(termName)

        val varDef = q"var $patName: $tpe = null"
        val defDef =
          q"""..$mods def $name[..$tparams](...$params): $tpe = {
             if ($termName != null) $termName else { $termName = $body; $termName }
          }"""

        q"{ $varDef ; $defDef }"
      case _ =>
        abort("@cache can only annotate method definitions")
    }
  }
}*/
