package packaged

import scala.gestalt._

package object macros2 {
  inline def plus(a: Int, b: Int): Int = meta {
    q"$a + $b"
  }

  object PlusObj {
    inline def plus(a: Int, b: Int): Int = meta {
      q"$a + $b"
    }
  }

  implicit class PlusFor(val a: Int) {
    inline def plus(b: Int): Int = meta {
      q"$this.a + $b"
    }
  }
}
