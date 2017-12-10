package packaged

import scala.gestalt._
import quasiquotes._

package object macros {
  def plus(a: Int, b: Int): Int = meta {
    tq"$a + $b"
  }

  object PlusObj {
    def plus(a: Int, b: Int): Int = meta {
      tq"$a + $b"
    }
  }

  implicit class PlusFor(val a: Int) {
    def plus(b: Int): Int = meta {
      tq"$this.a + $b"
    }
  }
}
