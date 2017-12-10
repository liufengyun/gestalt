package scala.gestalt.dotty

import gestalt.core

import dotty.tools.dotc
import dotc.core._
import Symbols.{ Symbol => DSymbol }

class Symbols(val toolbox: Toolbox) extends core.Symbols {
  import toolbox.types.{ Type, TypeRef, TermRef }
  import toolbox.denotations.Denotation

  implicit val ctx: Contexts.Context = toolbox.ctx

  type Symbol = DSymbol

  /** name of a member */
  def name(mem: Symbol): String = mem.showName

  /** type of a member with respect to a prefix */
  def asSeenFrom(mem: Symbol, prefix: Type): Type = mem.asSeenFrom(prefix).info

  def typeRef(sym: Symbol): TypeRef = sym.typeRef
  def termRef(sym: Symbol): TermRef = sym.termRef

  def isCase(sym: Symbol): Boolean = sym.is(Flags.Case)
  def isTrait(sym: Symbol): Boolean = sym.is(Flags.Trait)
  def isPrivate(sym: Symbol): Boolean = sym.is(Flags.Private)
  def isProtected(sym: Symbol): Boolean = sym.is(Flags.Protected)
  def isOverride(sym: Symbol): Boolean = sym.is(Flags.Override)
  def isFinal(sym: Symbol): Boolean = sym.is(Flags.Final)
  def isImplicit(sym: Symbol): Boolean = sym.is(Flags.Implicit)
  def isLazy(sym: Symbol): Boolean = sym.is(Flags.Lazy)
  def isSealed(sym: Symbol): Boolean = sym.is(Flags.Sealed)
  def isAbstract(sym: Symbol): Boolean = sym.is(Flags.Abstract)
  def isMutable(sym: Symbol): Boolean = sym.is(Flags.Mutable)
}
