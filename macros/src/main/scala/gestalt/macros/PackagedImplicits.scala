package packaged.macros

import scala.gestalt._

object ImplicitsForNumbers {

  implicit class PlusFor(val a: Int) {
    def plus(b: Int): Any = meta {
      import toolbox._
      q"$this.a + $b"
    }
  }

}
