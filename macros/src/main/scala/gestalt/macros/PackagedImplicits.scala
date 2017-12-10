package packaged.macros

import scala.gestalt._
import quasiquotes._

object ImplicitsForNumbers {

  implicit class PlusFor(val a: Int) {
    def plus(b: Int): Any = meta {
      tq"$this.a + $b"
    }
  }

}
