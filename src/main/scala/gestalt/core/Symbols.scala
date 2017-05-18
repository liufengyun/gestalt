package scala.gestalt.core

trait Symbols { this: Toolbox =>
  type Symbol <: AnyRef

  def Symbol: SymbolImpl
  trait SymbolImpl {
    /** name of a member */
    def name(mem: Symbol): String

    /** type of a member with respect to a prefix */
    def asSeenFrom(mem: Symbol, prefix: Type): Type
  }
}
