import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq

import scala.gestalt._

class modsTest extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"$mods object $name" = q"private object A"
    assert(mods.is(flags.Private))

    val q"$mods1 class $name2 $mods2 ($params)" = q"case class A private[core](x: Int)"
    assert(mods1.is(flags.Case))
    assert(mods2.is(flags.Private))
    assert(mods2.privateWithin == "core")

    defn
  }
}
