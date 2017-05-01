package scala.gestalt

trait Types { this: Toolbox =>
  type Type

  /** pretty print type */
  def show(tp: Type): String

  /** are the two types equal? */
  def =:=(tp1: Type, tp2: Type): Boolean

  /** is `tp1` a subtype of `tp2` */
  def <:<(tp1: Type, tp2: Type): Boolean

  /** returning a type referring to a type definition */
  def typeRef(path: String): Type

  /** returning a type referring to a value definition */
  def termRef(path: String): Type

  /** type associated with the tree */
  def typeOf(tree: Tree): Type

  /** does the type refer to a case class? */
  def isCaseClass(tp: Type): Boolean

  /** fields of a case class type -- only the ones declared in primary constructor */
  def caseFields(tp: Type): Seq[Symbol]

  /** field with the given name directly declared in the class */
  def fieldIn(tp: Type, name: String): Option[Symbol]

  /** fields directly declared in the class */
  def fieldsIn(tp: Type): Seq[Symbol]

  /** get non-private named methods defined directly inside the class */
  def methodIn(tp: Type, name: String): Seq[MethodSymbol]

  /** get all non-private methods defined directly inside the class, exluding constructors */
  def methodsIn(tp: Type): Seq[MethodSymbol]

  /** get named non-private methods declared or inherited */
  def method(tp: Type, name: String): Seq[MethodSymbol]

  /** get all non-private methods declared or inherited */
  def methods(tp: Type): Seq[MethodSymbol]

  /** get members directly declared or inherited that satisfy the predicate */
  // def members(tp: Type, pred: Symbol => Boolean = s => true): Seq[Symbol]
}

