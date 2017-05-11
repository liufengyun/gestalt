package scala.gestalt

trait Symbols { this: Toolbox =>
  type Symbol

  /** owner of current macro expansion */
  def owner: Symbol

  /** create a new symbol with current owner */
  def newValSymbol(name: String, info: Type): Symbol

  implicit class SymbolOps(sym: Symbol) {
    def name: String = Symbol.name(sym)
    def asSeenFrom(prefix: Type): Type = Symbol.asSeenFrom(sym, prefix)
  }

  val Symbol: SymbolImpl
  trait SymbolImpl {
    /** name of a member */
    def name(mem: Symbol): String

    /** type of a member with respect to a prefix */
    def asSeenFrom(mem: Symbol, prefix: Type): Type

    /** subst symbols in tree */
    def subst(tree: tpd.Tree)(from: List[Symbol], to: List[Symbol]): tpd.Tree

    /** change owner of the tree */
    def changeOwner(tree: tpd.Tree)(from: Symbol, to: Symbol): tpd.Tree
  }
}
