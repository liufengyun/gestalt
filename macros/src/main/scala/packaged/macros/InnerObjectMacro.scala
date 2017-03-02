package packaged.macros
import scala.gestalt._

object InnerObjectMacro {
  class Inner {
    inline def plus(a: Int, b: Int) = meta {
      q"$a + $b"
    }
  }

  object InnerObject {
    inline def plus(a: Int, b: Int) = meta {
      q"$a + $b"
    }
  }

}
