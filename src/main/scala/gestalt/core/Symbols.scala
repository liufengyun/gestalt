package scala.gestalt.core

trait Symbols {
  val toolbox: Toolbox
  import toolbox.types.{ TypeRef, TermRef }
  import toolbox._

  type Symbol <: AnyRef

  /** name of a member */
  def name(mem: Symbol): String

  /** type of a member with respect to a prefix */
  def asSeenFrom(mem: Symbol, prefix: Type): Type

  def typeRef(sym: Symbol): TypeRef
  def termRef(sym: Symbol): TermRef

  def isCase(sym: Symbol): Boolean
  def isTrait(sym: Symbol): Boolean
  def isPrivate(sym: Symbol): Boolean
  def isProtected(sym: Symbol): Boolean
  def isOverride(sym: Symbol): Boolean
  def isFinal(sym: Symbol): Boolean
  def isImplicit(sym: Symbol): Boolean
  def isLazy(sym: Symbol): Boolean
  def isSealed(sym: Symbol): Boolean
  def isAbstract(sym: Symbol): Boolean
  def isMutable(sym: Symbol): Boolean
}
