package scala.gestalt
package decos

import api._

trait Denotations {
  implicit class DenotationOps(denot: Denotation) {
    def name: String = Denotation.name(denot)
    def info: Type = Denotation.info(denot)
    def symbol: Symbol = Denotation.symbol(denot)
  }
}
