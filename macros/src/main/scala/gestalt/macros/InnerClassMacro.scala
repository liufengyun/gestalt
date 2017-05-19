package packaged.macros

import scala.gestalt.api._

class InnerClassMacro {
  def createInner = new Inner()
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
