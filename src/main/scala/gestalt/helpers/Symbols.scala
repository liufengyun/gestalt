package scala.gestalt
package helpers

import gestalt.api._

trait Symbols {
  implicit class SymbolOps(sym: Symbol) {
    def name: String = Symbol.name(sym)
    def asSeenFrom(prefix: Type): Type = Symbol.asSeenFrom(sym, prefix)
    def termRef: TermRef = Symbol.termRef(sym)
    def typeRef: TypeRef = Symbol.typeRef(sym)
    def isCase: Boolean = Symbol.isCase(sym)
    def isTrait: Boolean = Symbol.isTrait(sym)
    def isPrivate: Boolean = Symbol.isPrivate(sym)
    def isProtected: Boolean = Symbol.isProtected(sym)
    def isOverride: Boolean = Symbol.isOverride(sym)
    def isFinal: Boolean = Symbol.isFinal(sym)
    def isImplicit: Boolean = Symbol.isImplicit(sym)
    def isLazy: Boolean = Symbol.isLazy(sym)
    def isSealed: Boolean = Symbol.isSealed(sym)
    def isAbstract: Boolean = Symbol.isAbstract(sym)
    def isMutable: Boolean = Symbol.isMutable(sym)
  }
}