package scala.gestalt

trait Symbols {
  type Symbol

  val types: Types
  import types._

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
  }
}
