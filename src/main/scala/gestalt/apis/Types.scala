package scala.gestalt
package apis

object Types  {
  import api.{ toolbox => impl, XtensionBang, tpd, Symbol, Denotation }

  type Type >: Null <: AnyRef
  type TermRef <: Type
  type TypeRef <: Type
  type MethodType >: Null <: Type
  type ParamRef   >: Null <: Type

  /** pretty print type */
  def show(tp: Type): String = impl.types.show(!tp)

  /** are the two types equal? */
  def =:=(tp1: Type, tp2: Type): Boolean = impl.types.=:=(!tp1, !tp2)

  /** is `tp1` a subtype of `tp2` */
  def <:<(tp1: Type, tp2: Type): Boolean = impl.types.<:<(!tp1, !tp2)

  /** least upper bound of two types */
  def lub(tp1: Type, tp2: Type): Type = !impl.types.lub(!tp1, !tp2)

  /** returning a type referring to a global type definition */
  def typeRef(path: String): TypeRef = !impl.types.typeRef(path)

  /** returning a type referring to a global value definition */
  def termRef(path: String): TermRef = !impl.types.termRef(path)

  /** class symbol associated with the type */
  def classSymbol(tp: Type): Option[Symbol] = !impl.types.classSymbol(!tp)

  /** fields of a case class type -- only the ones declared in primary constructor */
  def caseFields(tp: Type): List[Denotation] = !impl.types.caseFields(!tp)

  /** field with the given name directly declared in the class */
  def fieldIn(tp: Type, name: String): Option[Denotation] = !impl.types.fieldIn(!tp, name)

  /** fields directly declared in the class */
  def fieldsIn(tp: Type): List[Denotation] = !impl.types.fieldsIn(!tp)

  /** get non-private named methods defined directly inside the class */
  def methodIn(tp: Type, name: String): List[Denotation] = !impl.types.methodIn(!tp, name)

  /** get all non-private methods defined directly inside the class, exluding constructors */
  def methodsIn(tp: Type): List[Denotation] = !impl.types.methodsIn(!tp)

  /** get named non-private methods declared or inherited */
  def method(tp: Type, name: String): List[Denotation] = !impl.types.method(!tp, name)

  /** get all non-private methods declared or inherited */
  def methods(tp: Type): List[Denotation] = !impl.types.methods(!tp)

  /** If `tp` points to a class, the module class of its companion object.
    *  If `tp` points to an object, its companion class.
    */
  def companion(tp: Type): Option[Type] = !impl.types.companion(!tp)

  /** widen singleton and constant types */
  def widen(tp: Type): Type = !impl.types.widen(!tp)

  /** denotation associated with the type */
  def denot(tp: Type): Option[Denotation] = !impl.types.denot(!tp)

  /** Turn a type into a typed tree */
  def toTree(tp: Type): tpd.Tree = !impl.types.toTree(!tp)

  /** Infer an implicit instance of the given type */
  def infer(tp: Type): Option[tpd.Tree] = !impl.types.infer(!tp)

  /*-------------------- type extractors ---------------------*/

  /** The type representing  =>T */
  object ByNameType {
    def unapply(tp: Type): Option[Type] =
      !impl.types.ByNameType.unapply(!tp)
  }

  /** The type representing  T[U1, ..., Un] */
  object AppliedType {
    def apply(tpcon: Type, args: List[Type]): Type =
      !impl.types.AppliedType(!tpcon, !args)

    def unapply(tp: Type): Option[(Type, List[Type])] =
      !impl.types.AppliedType.unapply(!tp)
  }

  /** The type representing (T1, T2, ...)R */
  object MethodType {
    def paramInfos(tp: MethodType): List[Type] =
      !impl.types.MethodType.paramInfos(!tp)

    def instantiate(tp: MethodType)(params: List[Type]): Type =
      !impl.types.MethodType.instantiate(!tp)(!params)

    def unapply(tp: Type): Option[MethodType] =
      !impl.types.MethodType.unapply(!tp)

    def apply(paramNames: List[String])
             (paramInfosExp: List[ParamRef] => List[Type],
              resultTypeExp: List[ParamRef] => Type): MethodType =
      !impl.types.MethodType(paramNames)(!paramInfosExp, !resultTypeExp)
  }
}
