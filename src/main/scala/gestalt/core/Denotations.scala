package scala.gestalt.core

trait Denotations { this: Toolbox =>
  type Denotation

  def Denotation: DenotationImpl
  trait DenotationImpl {
    def name(denot: Denotation): String

    def info(denot: Denotation): Type

    def symbol(denot: Denotation): Symbol
  }
}
