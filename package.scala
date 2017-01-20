package scala.meta

package object gestalt {
  // platform-independent quasiquote implementation based on standard constructors and extractors
  implicit class QuasiquoteHelper(val sc: StringContext) {
    def q(args: Any*): Any = ???
  }
}
