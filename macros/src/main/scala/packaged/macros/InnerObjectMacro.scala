package packaged.macros
import scala.gestalt._

object InnerObjectMacro {
  class Inner {
    def plus(a: Int, b: Int) = meta {
      q"$a + $b"
    }
  }

  object InnerObject {
    def plus(a: Int, b: Int) = meta {
      q"$a + $b"
    }
  }

}
