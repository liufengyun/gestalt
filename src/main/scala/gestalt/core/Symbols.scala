package scala.gestalt
package core

trait Symbols { this: Toolbox =>
  type Symbol <: AnyRef

  val Symbol: SymbolImpl
  trait SymbolImpl {
     /** create a new symbol with current owner */
    def newValSymbol(name: String, info: Type): Symbol

    /** name of a member */
    def name(mem: Symbol): String

    /** type of a member with respect to a prefix */
    def asSeenFrom(mem: Symbol, prefix: Type): Type
  }
}
