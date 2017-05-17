package scala.gestalt
package core

trait Denotations { this: Toolbox =>
  type Denotation

  val Denotation: DenotationImpl
  trait DenotationImpl {
    def name(denot: Denotation): String

    def info(denot: Denotation): Type

    def symbol(denot: Denotation): Symbol
  }
}
