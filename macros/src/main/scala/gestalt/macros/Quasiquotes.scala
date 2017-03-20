import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq

import scala.gestalt._

class modsTest extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"$mods object $name" = q"private object A"
    println(mods.toString)
    assert(mods.is(flags.Private))

    // val q"$mods class $name" = q"case class A(x: Int)"
    // assert(mods.is(flags.Case))

    defn
  }
}
