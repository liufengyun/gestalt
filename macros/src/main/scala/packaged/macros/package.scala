package packaged

import scala.gestalt._

package object macros {
  implicit class PlusFor(val a: Int) {
    inline def plus(b: Int): Any = meta {
      q"$this.a + $b"
    }
  }
}
