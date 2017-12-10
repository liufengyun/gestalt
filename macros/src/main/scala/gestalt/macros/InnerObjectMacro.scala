package packaged.macros
import scala.gestalt._
import quasiquotes._

object InnerObjectMacro {
  class Inner {
    def plus(a: Int, b: Int) = meta {
      tq"$a + $b"
    }
  }

  object InnerObject {
    def plus(a: Int, b: Int) = meta {
      tq"$a + $b"
    }
  }

}
