package scala

package object gestalt {
  /** The dummy Toolbox for type checking
   */
  implicit val toolbox: Toolbox = null

  /** The marker for meta block to highlight the different semantics
   */
  def meta(body: toolbox.Tree): Nothing = ???

  /**
    * Placeholder for prefix to show it before macro expansion
    * @see [[dotty.tools.dotc.macros.Transform]]
    */
  def prefix: toolbox.Tree = throw new IllegalAccessError("prefix can only be accessed from within a meta block")

  /** Placeholder of quasiquotes for type checking
   */
  implicit class QuasiquoteHelper(val sc: StringContext) {
    object q {
      def apply(args: Any*): toolbox.Tree = ???
      def unapply(tree: toolbox.Tree): Any = ???
    }

    object t {
      def apply(args: Any*): toolbox.Tree = ???
      def unapply(tree: toolbox.Tree): Any = ???
    }
  }

  /** Quasiquote implementation based on standard constructors and extractors
   *
   *  This method is intended to be reflectively called by the compiler
   */
  def expand(t: Toolbox)(label: String, parts: List[String], unquotes: List[t.Tree], isPattern: Boolean): t.Tree = {
    val quote      = new Quasiquote(t, "toolbox")
    val argsTyped  = unquotes.asInstanceOf[List[quote.t.Tree]]
    quote.expand(label, parts, argsTyped, isPattern).asInstanceOf[t.Tree]
  }

}
