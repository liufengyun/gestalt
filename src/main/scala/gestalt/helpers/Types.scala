package scala.gestalt
package helpers

import api._

trait Types {
  def MethodType(params: List[(String, Type)], retTp: Type): Type =
    api.MethodType(params.map(_._1))(_ => params.map(_._2), _ => retTp)

  implicit class TypeOps(tp: Type) {
    def =:=(tp2: Type) = Type.=:=(tp, tp2)
    def <:<(tp2: Type) = Type.<:<(tp, tp2)
    def isCaseClass = Type.classSymbol(tp) match {
      case Some(cls) => Symbol.isCase(cls)
      case None      => false
    }
    def caseFields: List[Denotation] = Type.caseFields(tp)
    def fieldIn(name: String): Option[Denotation] = Type.fieldIn(tp, name)
    def fieldsIn: List[Denotation] = Type.fieldsIn(tp)
    def methodIn(name: String): List[Denotation] = Type.methodIn(tp, name)
    def methodsIn: List[Denotation] = Type.methodsIn(tp)
    def method(name: String): List[Denotation] = Type.method(tp, name)
    def methods: List[Denotation] = Type.methods(tp)
    def companion: Option[Type] = Type.companion(tp)
    def show: String = Type.show(tp)
    def widen: Type = Type.widen(tp)
    def denot: Option[Denotation] = Type.denot(tp)
    def termSymbol: Option[Symbol] = denot.map(Denotation.symbol)
    def classSymbol: Option[Symbol] = Type.classSymbol(tp)
    def appliedTo(args: Type*): Type = AppliedType(tp, args.toList)
    def toTree: tpd.Tree = Type.toTree(tp)
  }

  implicit class MethodTypeOps(tp: MethodType) {
    def paramInfos: List[Type] = api.MethodType.paramInfos(tp)
    def instantiate(params: List[Type]): Type = api.MethodType.instantiate(tp)(params)
  }

  implicit class TermRefOps(tp: TermRef) {
    def symbol: Symbol    = Denotation.symbol(denot)
    def denot: Denotation = Type.denot(tp).get
  }
}