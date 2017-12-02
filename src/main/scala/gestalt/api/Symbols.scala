package scala.gestalt.api

object Symbols {
  import scala.gestalt.{ toolbox => impl, _ }

  /** name of a member */
  def name(mem: Symbol): String = impl.symbols.name(!mem)

  /** type of a member with respect to a prefix */
  def asSeenFrom(mem: Symbol, prefix: Type): Type = !impl.symbols.asSeenFrom(!mem, !prefix)

  def typeRef(sym: Symbol): Type.TypeRef = !impl.symbols.typeRef(!sym)
  def termRef(sym: Symbol): Type.TermRef = !impl.symbols.termRef(!sym)

  def isCase(sym: Symbol): Boolean = impl.symbols.isCase(!sym)
  def isTrait(sym: Symbol): Boolean = impl.symbols.isTrait(!sym)
  def isPrivate(sym: Symbol): Boolean = impl.symbols.isPrivate(!sym)
  def isProtected(sym: Symbol): Boolean = impl.symbols.isProtected(!sym)
  def isOverride(sym: Symbol): Boolean = impl.symbols.isOverride(!sym)
  def isFinal(sym: Symbol): Boolean = impl.symbols.isFinal(!sym)
  def isImplicit(sym: Symbol): Boolean = impl.symbols.isImplicit(!sym)
  def isLazy(sym: Symbol): Boolean = impl.symbols.isLazy(!sym)
  def isSealed(sym: Symbol): Boolean = impl.symbols.isSealed(!sym)
  def isAbstract(sym: Symbol): Boolean = impl.symbols.isAbstract(!sym)
  def isMutable(sym: Symbol): Boolean = impl.symbols.isMutable(!sym)
}
