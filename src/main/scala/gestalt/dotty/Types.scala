package scala.gestalt.dotty

import gestalt.core
import dotty.tools.dotc
import dotc.core.{ Types => DTypes, _ }
import dotc.ast.{ tpd => t }
import Decorators._
import dotc.util.Positions.Position

class Types(val toolbox: Toolbox) extends core.Types  {
  import toolbox.symbols.Symbol
  import toolbox.denotations.Denotation
  import toolbox.{ enclosingPosition, tpd }

  implicit val ctx: Contexts.Context = toolbox.ctx

  type Type       = DTypes.Type
  type ParamRef   = DTypes.ParamRef
  type TermRef    = DTypes.TermRef
  type TypeRef    = DTypes.TypeRef
  type MethodType = DTypes.MethodType

  /** pretty print type */
  def show(tp: Type): String = tp.show

  /** are the two types equal? */
  def =:=(tp1: Type, tp2: Type): Boolean = tp1 =:= tp2

  /** is `tp1` a subtype of `tp2` */
  def <:<(tp1: Type, tp2: Type): Boolean = tp1 <:< tp2

  def lub(tp1: Type, tp2: Type): Type = ctx.typeComparer.lub(tp1, tp2, false)

  /** returning a type referring to a type definition */
  def typeRef(path: String): TypeRef = ctx.staticRef(path.toTypeName, false).symbol.typeRef

  /** returning a type referring to a term definition */
  def termRef(path: String): TermRef = ctx.staticRef(path.toTermName, false).symbol.termRef

  def isCaseClass(tp: Type): Boolean = tp.classSymbol.is(Flags.Case)

  /** class symbol associated with the type */
  def classSymbol(tp: Type): Option[toolbox.symbols.Symbol] = tp.classSymbol match {
    case Symbols.NoSymbol => None
    case cls      => Some(cls)
  }

  /** val fields of a case class Type -- only the ones declared in primary constructor */
  def caseFields(tp: Type): List[Denotation] = {
    tp.memberDenots(
      DTypes.fieldFilter,
      (name, buf) => buf ++= tp.member(name).altsWith(_ is Flags.ParamAccessor)
    ).toList
  }

  /* field with the given name */
  def fieldIn(tp: Type, name: String): Option[Denotation] = {
    tp.memberExcluding(name.toTermName, Flags.Method).altsWith(
      p => p.owner == tp.widenSingleton.classSymbol
    ).headOption
  }

  def fieldsIn(tp: Type): List[Denotation] = {
    tp.memberDenots(
      DTypes.fieldFilter,
      (name, buf) => buf ++= tp.member(name).altsWith(
        p => !p.is(Flags.Method) && p.owner == tp.widenSingleton.classSymbol
      )
    ).toList
  }

  def method(tp: Type, name: String): List[Denotation] = {
    tp.member(name.toTermName).altsWith(p => p.is(Flags.Method))
  }

  def methods(tp: Type): List[Denotation] = {
    tp.memberDenots(
      DTypes.takeAllFilter,
      (name, buf) => buf ++= tp.member(name).altsWith(p => p.is(Flags.Method) && !p.isConstructor)
    ).toList
  }

  def methodIn(tp: Type, name: String): List[Denotation] = {
    tp.member(name.toTermName).altsWith(
      p => p.is(Flags.Method) && p.owner == tp.widenSingleton.classSymbol
    )
  }

  def methodsIn(tp: Type): List[Denotation] = {
    tp.memberDenots(
      DTypes.takeAllFilter,
      (name, buf) => buf ++= tp.member(name).altsWith(
        p => p.is(Flags.Method) && p.owner == tp.widenSingleton.classSymbol && !p.isConstructor
      )
    ).toList
  }

  def companion(tp: Type): Option[Type] = {
    val clazz = tp.widenSingleton.classSymbol
    if (clazz.exists)
      if (clazz.is(Flags.Module) && clazz.companionClass.exists)
        Some(clazz.companionClass.namedType)
      else if (!clazz.is(Flags.Module) && clazz.companionModule.exists)
        Some(clazz.companionModule.namedType)
      else None
    else None
  }

  def widen(tp: Type): Type = tp.deconst.widen

  def denot(tp: Type): Option[Denotation] = tp match {
    case tp: DTypes.NamedType => Some(tp.denot)
    case tp: DTypes.TypeProxy => denot(tp.underlying)
    case _ => None
  }

  def toTree(tp: Type): tpd.Tree = t.TypeTree(tp)

  /** Infer an implicit instance of the given type */
  def infer(tp: Type): Option[tpd.Tree] = {
    var hasError = false
    def implicitArgError(msg: String => String) = hasError = true

    val res = ctx.typer.inferImplicitArg(tp, implicitArgError, enclosingPosition)
    if (hasError) None
    else Some(res)
  }

  object ByNameType extends ByNameTypeImpl {
    def unapply(tp: Type): Option[Type] = tp match {
      case tp: DTypes.ExprType => Some(tp.underlying)
      case _ => None
    }
  }

  object AppliedType extends AppliedTypeImpl {
    def apply(tp: Type, args: List[Type]): Type = DTypes.AppliedType(tp, args)
    def unapply(tp: Type): Option[(Type, List[Type])] = tp match {
      case DTypes.AppliedType(tp, args) => Some(tp -> args)
      case _ => None
    }
  }

  object MethodType extends MethodTypeImpl {
    def paramInfos(tp: MethodType): List[Type] = tp.paramInfos
    def instantiate(tp: MethodType)(params: List[Type]): Type = {
      tp.instantiate(params)
    }
    def unapply(tp: Type): Option[MethodType] = tp match {
      case tp: DTypes.MethodType => Some(tp)
      case _ => None
    }

    def apply(paramNames: List[String])
             (paramInfosExp: List[ParamRef] => List[Type],
              resultTypeExp: List[ParamRef] => Type): MethodType = {
      val names = paramNames.map(_.toTermName)
      DTypes.MethodType(names)(
        mt => paramInfosExp(mt.paramRefs),
        mt => resultTypeExp(mt.paramRefs)
      )
    }
  }
}