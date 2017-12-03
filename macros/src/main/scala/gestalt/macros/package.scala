package packaged

import scala.gestalt._

package object macros {
  def plus(a: Int, b: Int): Int = meta {
    q"$a + $b"
  }

  object PlusObj {
    def plus(a: Int, b: Int): Int = meta {
      q"$a + $b"
    }
  }

  implicit class PlusFor(val a: Int) {
    def plus(b: Int): Int = meta {
      q"$this.a + $b"
    }
  }
}
