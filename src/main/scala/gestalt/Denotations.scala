package scala.gestalt

trait Denotations { self: Toolbox =>
  type Denotation

  implicit class DenotationOps(denot: Denotation) {
    def name: String = Denotation.name(denot)
    def info: Type = Denotation.info(denot)
    def symbol: Symbol = Denotation.symbol(denot)
  }

  val Denotation: DenotationImpl
  trait DenotationImpl {
    def name(denot: Denotation): String

    def info(denot: Denotation): Type

    def symbol(denot: Denotation): Symbol
  }
}
