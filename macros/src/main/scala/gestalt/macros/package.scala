package packaged

import scala.gestalt._

package object macros {
  def plus(a: Int, b: Int): Int = meta {
    import toolbox._
    q"$a + $b"
  }

  object PlusObj {
    def plus(a: Int, b: Int): Int = meta {
      import toolbox._
      q"$a + $b"
    }
  }

  implicit class PlusFor(val a: Int) {
    def plus(b: Int): Int = meta {
      import toolbox._
      q"$this.a + $b"
    }
  }
}
