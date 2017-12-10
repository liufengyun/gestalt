package scala.gestalt.dotty

import gestalt.core

import dotty.tools.dotc
import dotc.core._
import dotc.ast.{Trees => c, tpd => t, untpd => d}
import NameOps._
import Names.TermName
import Decorators._
import Constants._
import d.modsDeco


case class DottyModifiers(dottyMods: d.Modifiers) extends core.Modifiers {
  type Tree = d.Tree
  type ThisType = DottyModifiers

  def isPrivate: Boolean = dottyMods.is(Flags.Private)
  def isProtected: Boolean = dottyMods.is(Flags.Protected)
  def isOverride: Boolean = dottyMods.is(Flags.Override)
  def isFinal: Boolean = dottyMods.is(Flags.Final)
  def isImplicit: Boolean = dottyMods.is(Flags.Implicit)
  def isLazy: Boolean = dottyMods.is(Flags.Lazy)
  def isSealed: Boolean = dottyMods.is(Flags.Sealed)
  def isAbstract: Boolean = dottyMods.is(Flags.Abstract)
  def isCase: Boolean = dottyMods.is(Flags.Case)
  def isContravariant: Boolean = dottyMods.is(Flags.Contravariant)
  def isCovariant: Boolean = dottyMods.is(Flags.Covariant)
  def isInline: Boolean = dottyMods.is(Flags.Inline)
  def isMutable: Boolean = dottyMods.is(Flags.Mutable)

  def isValParam: Boolean =
    dottyMods.is(Flags.Param) &&
      !dottyMods.is(Flags.Mutable) &&
      !dottyMods.is(Flags.allOf(Flags.Private, Flags.Local))

  def isVarParam: Boolean =
    dottyMods.is(Flags.Param) &&
      dottyMods.is(Flags.Mutable) &&
      !dottyMods.is(Flags.allOf(Flags.Private, Flags.Local))

  // can be empty or `this`
  def setPrivate(within: String): ThisType =
    if (within == "this") DottyModifiers(dottyMods | Flags.Private | Flags.Local)
    else DottyModifiers(dottyMods.withPrivateWithin(within.toTypeName) | Flags.Private)

  def setProtected(within: String): ThisType =
    if (within == "this") DottyModifiers(dottyMods | Flags.Protected | Flags.Local)
    else DottyModifiers(dottyMods.withPrivateWithin(within.toTypeName) | Flags.Protected)

  def setOverride: ThisType = DottyModifiers(dottyMods | Flags.Override)
  def setFinal: ThisType = DottyModifiers(dottyMods | Flags.Final)
  def setImplicit: ThisType = DottyModifiers(dottyMods | Flags.Implicit)
  def setLazy: ThisType = DottyModifiers(dottyMods | Flags.Lazy)
  def setSealed: ThisType = DottyModifiers(dottyMods | Flags.Sealed)
  def setAbstract: ThisType = DottyModifiers(dottyMods | Flags.Abstract)
  def setCase: ThisType = DottyModifiers(dottyMods | Flags.Case)
  def setContravariant: ThisType = DottyModifiers(dottyMods | Flags.Contravariant)
  def setCovariant: ThisType = DottyModifiers(dottyMods | Flags.Covariant)
  def setInline: ThisType = DottyModifiers(dottyMods | Flags.Inline)
  def setMutable: ThisType = DottyModifiers(dottyMods | Flags.Mutable)

  def setValParam: ThisType = DottyModifiers(dottyMods &~ Flags.Local &~ Flags.Mutable)
  def setVarParam: ThisType = DottyModifiers(dottyMods &~ Flags.Local | Flags.Mutable)

  def withAddedAnnotation(annot: d.Tree): ThisType = DottyModifiers(dottyMods.withAddedAnnotation(annot))
  def withAnnotations(annots: List[d.Tree]): ThisType = DottyModifiers(dottyMods.withAnnotations(annots))

  def hasAnnotations: Boolean = dottyMods.hasAnnotations

  def annotations: List[Tree] = dottyMods.annotations

  def privateWithin: String =
    if (dottyMods.is(Flags.Local)) "this"
    else dottyMods.privateWithin.toString
}
