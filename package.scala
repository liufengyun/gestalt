package scala

package object gestalt {

  /** Placeholder for quasiquotes
   */
  implicit class QuasiquoteHelper(val sc: StringContext) {
    def q(args: Any*): Any = ???
    def t(args: Any*): Any = ???
  }

  /** Quasiquote implementation based on standard constructors and extractors
   *
   *  This method is intended to be reflectively called by the compiler
   */
  def expand(t: Toolbox)(label: String, parts: List[t.Tree], unquotes: List[t.Tree], isPattern: Boolean): t.Tree = ???
}
