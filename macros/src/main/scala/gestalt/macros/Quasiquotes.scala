import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq

import scala.gestalt._

class modsTest extends StaticAnnotation {
  def apply(defn: Any): Any = meta {
    val q"$mods object $name" = q"private object A"
    assert(mods.isPrivate)

    val q"$mods1 class $name2 $mods2 ($params)" = q"case class A private[core](x: Int)"
    println("mods1: " + mods1)
    println("mods2: " + mods2)
    assert(mods1.isCase)
    assert(mods2.isPrivate)
    assert(mods2.privateWithin == "core")

    defn
  }
}
