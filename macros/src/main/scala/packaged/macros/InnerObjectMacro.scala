package packaged.macros
import scala.gestalt._

/**
  * Created by valdis on 17.28.2.
  */
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
