package scala.gestalt.core

trait Modifiers {
  type Tree
  type ThisType <: Modifiers

  def isPrivate: Boolean
  def isProtected: Boolean
  def isOverride: Boolean
  def isFinal: Boolean
  def isImplicit: Boolean
  def isLazy: Boolean
  def isSealed: Boolean
  def isAbstract: Boolean
  def isValParam: Boolean
  def isVarParam: Boolean
  def isCase: Boolean
  def isContravariant: Boolean
  def isCovariant: Boolean
  def isInline: Boolean
  def isMutable: Boolean
  def privateWithin: String
  def hasAnnotations: Boolean

  // can be empty or `this`
  def setPrivate(within: String): ThisType
  def setProtected(within: String): ThisType
  def setOverride: ThisType
  def setFinal: ThisType
  def setImplicit: ThisType
  def setLazy: ThisType
  def setSealed: ThisType
  def setAbstract: ThisType
  def setValParam: ThisType
  def setVarParam: ThisType
  def setCase: ThisType
  def setContravariant: ThisType
  def setCovariant: ThisType
  def setInline: ThisType
  def setMutable: ThisType

  def withAddedAnnotation(annot: Tree): ThisType
  def withAnnotations(annots: List[Tree]): ThisType
  def annotations: List[Tree]
}