package packaged.macros

import scala.gestalt._

object ImplicitsForNumbers {

  implicit class PlusFor(val a: Int) {
    def plus(b: Int): Any = meta {
      q"$this.a + $b"
    }
  }

}
