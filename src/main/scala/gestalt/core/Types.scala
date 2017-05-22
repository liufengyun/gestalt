package scala.gestalt.core

trait Types extends MethodTypes { this: Toolbox =>
  type Type >: Null <: AnyRef
  type TermRef <: Type
  type TypeRef <: Type

  def Type: TypeImpl
  trait TypeImpl {
    /** pretty print type */
    def show(tp: Type): String

    /** are the two types equal? */
    def =:=(tp1: Type, tp2: Type): Boolean

    /** is `tp1` a subtype of `tp2` */
    def <:<(tp1: Type, tp2: Type): Boolean

    /** least upper bound of two types */
    def lub(tp1: Type, tp2: Type): Type

    /** returning a type referring to a global type definition */
    def typeRef(path: String): TypeRef

    /** returning a type referring to a global value definition */
    def termRef(path: String): TermRef

    /** class symbol associated with the type */
    def classSymbol(tp: Type): Option[Symbol]

    /** fields of a case class type -- only the ones declared in primary constructor */
    def caseFields(tp: Type): List[Denotation]

    /** field with the given name directly declared in the class */
    def fieldIn(tp: Type, name: String): Option[Denotation]

    /** fields directly declared in the class */
    def fieldsIn(tp: Type): List[Denotation]

    /** get non-private named methods defined directly inside the class */
    def methodIn(tp: Type, name: String): List[Denotation]

    /** get all non-private methods defined directly inside the class, exluding constructors */
    def methodsIn(tp: Type): List[Denotation]

    /** get named non-private methods declared or inherited */
    def method(tp: Type, name: String): List[Denotation]

    /** get all non-private methods declared or inherited */
    def methods(tp: Type): List[Denotation]

    /** If `tp` points to a class, the module class of its companion object.
     *  If `tp` points to an object, its companion class.
     */
    def companion(tp: Type): Option[Type]

    /** widen singleton types */
    def widen(tp: Type): Type

    /** denotation associated with the type */
    def denot(tp: Type): Option[Denotation]

    /** The type representing  T[U1, ..., Un] */
    def appliedTo(tp: Type, args: List[Type]): Type

    /** Turn a type into a typed tree */
    def toTree(tp: Type): tpd.Tree

    /** Infer an implicit instance of the given type */
    def infer(tp: Type): Option[tpd.Tree]
  }


  /*-------------------- type extractors ---------------------*/

  def ByNameType: ByNameTypeImpl
  trait ByNameTypeImpl {
    def unapply(tp: Type): Option[Type]
  }
}

trait MethodTypes { this: Types =>
  type MethodType >: Null <: Type

  def MethodType: MethodTypeImpl
  trait MethodTypeImpl {
    def paramInfos(tp: MethodType): List[Type]
    def instantiate(tp: MethodType)(params: List[Type]): Type
    def unapply(tp: Type): Option[MethodType]
  }
}
