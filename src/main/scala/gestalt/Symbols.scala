package scala.gestalt

trait Symbols { this: Toolbox =>
  type Symbol
  type MethodSymbol <: Symbol

  /** name of a member */
  def name(mem: Symbol): String

  /** type of a member with respect to a prefix */
  def asSeenFrom(mem: Symbol, prefix: Type): Type

  // def tparams(method: MethodSymbol): Seq[String]
  // def paramss(method: MethodSymbol): Seq[Seq[(name, Symbol)]]
}
