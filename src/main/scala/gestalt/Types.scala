package scala.gestalt

trait Types extends MethodTypes { this: Toolbox =>
  type Type >: Null <: AnyRef

  implicit class TypeOps(tp: Type) {
    def =:=(tp2: Type) = Type.=:=(tp, tp2)
    def <:<(tp2: Type) = Type.<:<(tp, tp2)
    def isCaseClass = Type.isCaseClass(tp)
    def caseFields: Seq[Denotation] = Type.caseFields(tp)
    def fieldIn(name: String): Option[Denotation] = Type.fieldIn(tp, name)
    def fieldsIn: Seq[Denotation] = Type.fieldsIn(tp)
    def methodIn(name: String): Seq[Denotation] = Type.methodIn(tp, name)
    def methodsIn: Seq[Denotation] = Type.methodsIn(tp)
    def method(name: String): Seq[Denotation] = Type.method(tp, name)
    def methods: Seq[Denotation] = Type.methods(tp)
    def companion: Option[Type] = Type.companion(tp)
    def show: String = Type.show(tp)
    def widen: Type = Type.widen(tp)
    def denot: Option[Denotation] = Type.denot(tp)
  }

  implicit class TreeTypeOps(tree: tpd.Tree) {
    def tpe: Type = Type.typeOf(tree)
    def hasType: Boolean = Type.hasType(tree)
  }

  val Type: TypeImpl
  trait TypeImpl {
    /** pretty print type */
    def show(tp: Type): String

    /** are the two types equal? */
    def =:=(tp1: Type, tp2: Type): Boolean

    /** is `tp1` a subtype of `tp2` */
    def <:<(tp1: Type, tp2: Type): Boolean

    /** returning a type referring to a global type definition */
    def typeRef(path: String): Type

    /** returning a type referring to a global value definition */
    def termRef(path: String): Type

    /** type associated with the tree */
    def typeOf(tree: tpd.Tree): Type

    /** whether the tree is typed or not
     *
     *  @note this is temporary, once we separate typed trees from untped
     *        trees, this should be removed.
     */
    def hasType(tree: tpd.Tree): Boolean

    /** does the type refer to a case class? */
    def isCaseClass(tp: Type): Boolean

    /** fields of a case class type -- only the ones declared in primary constructor */
    def caseFields(tp: Type): Seq[Denotation]

    /** field with the given name directly declared in the class */
    def fieldIn(tp: Type, name: String): Option[Denotation]

    /** fields directly declared in the class */
    def fieldsIn(tp: Type): Seq[Denotation]

    /** get non-private named methods defined directly inside the class */
    def methodIn(tp: Type, name: String): Seq[Denotation]

    /** get all non-private methods defined directly inside the class, exluding constructors */
    def methodsIn(tp: Type): Seq[Denotation]

    /** get named non-private methods declared or inherited */
    def method(tp: Type, name: String): Seq[Denotation]

    /** get all non-private methods declared or inherited */
    def methods(tp: Type): Seq[Denotation]

    /** If `tp` points to a class, the module class of its companion object.
     *  If `tp` points to an object, its companion class.
     */
    def companion(tp: Type): Option[Type]

    /** widen singleton types */
    def widen(tp: Type): Type

    /** denotation associated with the type */
    def denot(tp: Type): Option[Denotation]
  }


  /*-------------------- type extractors ---------------------*/

  val ByNameType: ByNameTypeImpl
  trait ByNameTypeImpl {
    def unapply(tp: Type): Option[Type]
  }
}

trait MethodTypes { this: Types =>
  type MethodType >: Null <: Type

  implicit class MethodTypeOps(tp: MethodType) {
    def paramInfos: Seq[Type] = MethodType.paramInfos(tp)
    def instantiate(params: Seq[Type]): Type = MethodType.instantiate(tp)(params)
  }

  val MethodType: MethodTypeImpl
  trait MethodTypeImpl {
    def paramInfos(tp: MethodType): Seq[Type]
    def instantiate(tp: MethodType)(params: Seq[Type]): Type
    def unapply(tp: Type): Option[MethodType]
  }
}

