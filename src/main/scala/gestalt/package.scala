package scala

package object gestalt {
  /** The dummy Toolbox for type checking
   */
  implicit val toolbox: Toolbox = null

  /** The marker for meta block to highlight the different semantics
   */
  def meta(body: toolbox.Tree): Nothing = ???

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
  def expand(t: Toolbox)(label: String, parts: List[t.Tree], unquotes: List[t.Tree], isPattern: Boolean): t.Tree = {
    val quote      = new Quasiquote(t, "toolbox")
    val partsTyped = parts.asInstanceOf[List[quote.t.Tree]]
    val argsTyped  = unquotes.asInstanceOf[List[quote.t.Tree]]
    quote.expand(label, partsTyped, argsTyped, isPattern).asInstanceOf[t.Tree]
  }


  // implicit helpers
  implicit def toOption(t: toolbox.Tree): Option[toolbox.Tree] = Some(t)
}
