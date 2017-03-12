package scala.gestalt.dotty

import scala.gestalt.Toolbox
import scala.collection.immutable.Seq

import dotty.tools.dotc._
import core._
import ast.{ untpd => d, Trees => c }
import StdNames._
import NameOps._
import Contexts._
import Decorators._
import Constants._
import d.modsDeco
import util.Positions
import Positions.Position

import scala.collection.mutable.ListBuffer

class DottyToolbox(enclosingPosition: Position = Positions.NoPosition)(implicit ctx: Context) extends Toolbox {
  type Tree = d.Tree
  type TypeTree = d.Tree
  type Type = Types.Type

  def =:=(tp1: Type, tp2: Type): Boolean = ???
  def <:<(tp1: Type, tp2: Type): Boolean = ???
  def typeOf(path: String): Type = ???

  def getOrEmpty(treeOpt: Option[Tree]): Tree = treeOpt.getOrElse(d.EmptyTree)

  private implicit def fromMods(mods: Seq[Tree]): d.Modifiers = {
    def addMod(modifiers: d.Modifiers, mod: d.Mod): d.Modifiers =
      modifiers.withAddedMod(mod) | mod.flags

    mods.foldLeft(d.Modifiers()) { (modifiers, mod) =>
      mod match {
        case Mod.Annot(body) =>
          modifiers.withAddedAnnotation(body)
        case Mod.Private(within) =>
          val modifiers2 = within match {
            case "this" => modifiers | Flags.Local
            case  _     => modifiers.withPrivateWithin(within.toTypeName)
          }
          addMod(modifiers2, d.Mod.Private())
        case Mod.Protected(within) =>
          val modifiers2 = within match {
            case "this" => modifiers | Flags.Local
            case  _     => modifiers.withPrivateWithin(within.toTypeName)
          }
          addMod(modifiers2, d.Mod.Protected())
        case Mod.Implicit() =>
          addMod(modifiers, d.Mod.Implicit())
        case Mod.Final() =>
          addMod(modifiers, d.Mod.Final())
        case Mod.Sealed() =>
          addMod(modifiers, d.Mod.Sealed())
        case Mod.Override() =>
          addMod(modifiers, d.Mod.Override())
        case Mod.Case() =>
          modifiers | Flags.Case
        case Mod.Abstract() =>
          addMod(modifiers, d.Mod.Abstract())
        case Mod.Covariant() =>
          modifiers | Flags.Covariant
        case Mod.Contravariant() =>
          modifiers | Flags.Contravariant
        case Mod.Lazy() =>
          addMod(modifiers, d.Mod.Lazy())
        case Mod.Val() =>
          addMod(modifiers, d.Mod.Val())
        case Mod.Var() =>
          addMod(modifiers, d.Mod.Var())
        case Mod.Inline() =>
          addMod(modifiers, d.Mod.Inline())
      }
    }
  }

  private def toMods(modifiers: d.Modifiers): List[Tree] = {
    val lb = new ListBuffer[Tree]

    if (modifiers.hasAnnotations) {
      lb ++= modifiers.annotations.map(Mod.Annot(_))
    }

    if (modifiers.is(Flags.Case))
      lb += Mod.Case()

    import d.{Mod => mod}
    lb ++= modifiers.mods.map { modScala =>
      modScala match {
        case _: mod.Override =>
          Mod.Override()
        case _: mod.Abstract =>
          Mod.Abstract()
        case _: mod.Final =>
          Mod.Final()
        case _: mod.Implicit =>
          Mod.Implicit()
        case _: mod.Inline =>
          Mod.Inline()
        case _: mod.Lazy =>
          Mod.Lazy()
        case _: mod.Private =>
          if (modifiers.hasPrivateWithin)
            Mod.Private(modifiers.privateWithin.toString)
          else if (modifiers is Flags.Local)
            Mod.Private("this")
          else
            Mod.Private("")
        case _: mod.Protected =>
          if (modifiers.hasPrivateWithin)
            Mod.Protected(modifiers.privateWithin.toString)
          else if (modifiers is Flags.Local)
            Mod.Protected("this")
          else
            Mod.Protected("")
        case _: mod.Sealed =>
          Mod.Sealed()
        case _: mod.Type =>
          Mod.Type()
        case _: mod.Val =>
          Mod.Val()
        case _: mod.Var =>
          Mod.Var()
      }
    }

    if (modifiers.is(Flags.Covariant)) lb += Mod.Covariant()
    if (modifiers.is(Flags.Contravariant)) lb += Mod.Contravariant()

    lb.toList
  }

  object Object extends ObjectHelper {
    def apply(mods: Seq[Tree], name: String, parents: Seq[Tree], selfOpt: Option[Tree], stats: Seq[Tree]): Tree = {
      val constr = d.DefDef(nme.CONSTRUCTOR, Nil, Nil, d.TypeTree(), d.EmptyTree)
      val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
      val templ = d.Template(constr, parents.toList, self, stats)
      d.ModuleDef(name.toTermName, templ).withMods(fromMods(mods)).withPos(enclosingPosition)
    }

    def unapply(tree: Tree): Option[(Seq[Tree], String, Seq[Tree], Option[Tree], Seq[Tree])] = tree match {
      case obj @ d.ModuleDef(name, templ @ c.Template(constr, parents, self: d.ValDef, body)) =>
        val selfOpt = if (self.name == nme.WILDCARD || self.isEmpty) None else Some(Self(self.name.toString, self.tpt))
        Some((toMods(obj.mods), name.toString, parents, selfOpt, templ.body))
      case _ => None
    }
  }

  object Class extends ClassHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], ctor: Option[Tree], parents: Seq[Tree], selfOpt: Option[Tree], stats: Seq[Tree]): Tree = {
      val constr =
        if (ctor.isEmpty) d.DefDef(nme.CONSTRUCTOR, Nil, Nil, d.TypeTree(), d.EmptyTree)
        else {
          val PrimaryCtor(mods, paramss) = ctor.get
          val tparamsCast = tparams.toList.asInstanceOf[List[d.TypeDef]]
          val paramssCast = paramss.map(_.toList).toList.asInstanceOf[List[List[d.ValDef]]]
          d.DefDef(nme.CONSTRUCTOR, tparamsCast, paramssCast, d.TypeTree(), d.EmptyTree).withMods(fromMods(mods))
        }

      val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
      val templ = d.Template(constr, parents.toList, self, stats)
      d.TypeDef(name.toTypeName, templ).withMods(fromMods(mods)).withPos(enclosingPosition)
    }

    def unapply(tree: Tree): Option[(Seq[Tree], String, Seq[Tree], Option[Tree], Seq[Tree], Option[Tree], Seq[Tree])] = tree match {
      case cdef @ c.TypeDef(name, templ @ c.Template(constr, parents, self, body)) =>
        var tparams: List[Tree] = Nil
        val ctor = constr match {
          case c.DefDef(nme.CONSTRUCTOR, Nil, Nil, c.TypeTree(), d.EmptyTree) => None
          case pctor @ c.DefDef(nme.CONSTRUCTOR, tps, paramss, c.TypeTree(), d.EmptyTree) =>
            tparams = tps
            Some(PrimaryCtor(toMods(pctor.mods), paramss))
        }
        val selfOpt = if (self.name == nme.WILDCARD && self.tpt == d.TypeTree()) None else Some(Self(self.name.toString, self.tpt))
        Some((toMods(cdef.mods), name.toString, tparams, ctor, tparams, selfOpt, templ.body))  // TODO: parents
      case _ => None
    }
  }

  object AnonymClass extends AnonymClassHelper {
    def apply(parents: Seq[Tree], selfOpt: Option[Tree], stats: Seq[Tree]): Tree = {
      val init = d.DefDef(nme.CONSTRUCTOR, List(), List(), d.TypeTree(), d.EmptyTree)
      val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
      d.Template(init, parents.toList, self, stats).withPos(enclosingPosition)
    }
  }

  object Trait extends TraitHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], ctor: Option[Tree], parents: Seq[Tree], self: Option[Tree], stats: Seq[Tree]): Tree =
      Class(mods, name, tparams, ctor, parents, self, stats).asInstanceOf[d.TypeDef].withFlags(Flags.Trait).withPos(enclosingPosition)

    def unapply(tree: Tree): Option[(Seq[Tree], String, Seq[Tree], Option[Tree], Seq[Tree], Option[Tree], Seq[Tree])] =
      if (!tree.isInstanceOf[d.TypeDef]) return None
      else {
        val typedef = tree.asInstanceOf[d.TypeDef]
        if (!typedef.mods.is(Flags.Trait)) return None

        Class.unapply(typedef)
      }
  }

  object TypeDecl extends TypeDeclHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], tboundsOpt: Option[TypeTree]): Tree = {
      val tbounds = tboundsOpt.getOrElse(d.TypeBoundsTree(d.EmptyTree, d.EmptyTree))
      val body =
        if (tparams.size == 0) tbounds
        else d.PolyTypeTree(tparams.toList.asInstanceOf[List[d.TypeDef]], tbounds)
      d.TypeDef(name.toTypeName, body).withMods(fromMods(mods)).withPos(enclosingPosition)
    }
  }

  object TypeAlias extends TypeAliasHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], rhs: TypeTree): Tree = {
      val body =
        if (tparams.size == 0) rhs
        else d.PolyTypeTree(tparams.toList.asInstanceOf[List[d.TypeDef]], rhs)
      d.TypeDef(name.toTypeName, body).withMods(fromMods(mods))
    }
  }

  object DefDef extends DefDefHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], paramss: Seq[Seq[Tree]], tpe: Option[TypeTree], rhs: Tree): Tree = {
      val types = tparams.toList.asInstanceOf[List[d.TypeDef]]
      val params = paramss.map(_.toList).toList.asInstanceOf[List[List[d.ValDef]]]
      d.DefDef(name.toTermName, types, params, tpe.getOrElse(d.TypeTree()), rhs).withMods(fromMods(mods))
    }
  }

  object DefDecl extends DefDeclHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], paramss: Seq[Seq[Tree]], tpe: TypeTree): Tree = {
      val types = tparams.toList.asInstanceOf[List[d.TypeDef]]
      val params = paramss.map(_.toList).toList.asInstanceOf[List[List[d.ValDef]]]
      d.DefDef(name.toTermName, types, params, tpe, d.EmptyTree).withMods(fromMods(mods))
    }
  }

  object ValDef extends ValDefHelper {
    def apply(mods: Seq[Tree], name: String, tpe: Option[TypeTree], rhs: Tree): Tree =
      d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), rhs).withMods(fromMods(mods))

    def apply(mods: Seq[Tree], lhs: Tree, tpe: Option[TypeTree], rhs: Tree): Tree =
      d.PatDef(fromMods(mods), List(lhs), tpe.getOrElse(d.TypeTree()), rhs)

    def apply(mods: Seq[Tree], pats: Seq[Tree], tpe: Option[TypeTree], rhs: Tree): Tree =
      d.PatDef(fromMods(mods), pats.toList, tpe.getOrElse(d.TypeTree()), rhs)
  }

  object ValDecl extends ValDeclHelper {
    def apply(mods: Seq[Tree], name: String, tpe: TypeTree): Tree =
      d.ValDef(name.toTermName, tpe, d.EmptyTree).withMods(fromMods(mods))

    def apply(mods: Seq[Tree], vals: Seq[String], tpe: TypeTree): Tree =
      d.PatDef(fromMods(mods), vals.map(n => d.Ident(n.toTermName)).toList, tpe, d.EmptyTree)
  }

  object VarDef extends VarDefHelper {
    def apply(mods: Seq[Tree], name: String, tpe: Option[TypeTree], rhs: Tree): Tree =
      d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), rhs).withMods(fromMods(mods) | Flags.Mutable)

    def apply(mods: Seq[Tree], lhs: Tree, tpe: Option[TypeTree], rhs: Tree): Tree =
      d.PatDef(fromMods(mods) | Flags.Mutable, List(lhs), tpe.getOrElse(d.TypeTree()), rhs)

    def apply(mods: Seq[Tree], pats: Seq[Tree], tpe: Option[TypeTree], rhs: Tree): Tree =
      d.PatDef(fromMods(mods) | Flags.Mutable, pats.toList, tpe.getOrElse(d.TypeTree()), rhs)
  }

  object VarDecl extends VarDeclHelper {
    def apply(mods: Seq[Tree], name: String, tpe: TypeTree): Tree =
      d.ValDef(name.toTermName, tpe, d.EmptyTree).withMods(fromMods(mods) | Flags.Mutable)

    def apply(mods: Seq[Tree], vals: Seq[String], tpe: TypeTree): Tree =
      d.PatDef(fromMods(mods) | Flags.Mutable, vals.map(n => d.Ident(n.toTermName)).toList, tpe, d.EmptyTree)
  }

  object PrimaryCtor extends PrimaryCtorHelper {
    // Dummy trees to retrofit Dotty AST
    private case class PrimaryCtorTree(mods: Seq[Tree], paramss: Seq[Seq[Tree]]) extends d.Tree

    def apply(mods: Seq[Tree], paramss: Seq[Seq[Tree]]): Tree = PrimaryCtorTree(mods, paramss).withPos(enclosingPosition)

    def unapply(tree: Tree): Option[(Seq[Tree], Seq[Seq[Tree]])] = tree match {
      case PrimaryCtorTree(mods, paramss) => Some((mods, paramss))
      case _                              => None
    }
  }

  object SecondaryCtor extends SecondaryCtorHelper {
    def apply(mods: Seq[Tree], paramss: Seq[Seq[Tree]], rhs: Tree): Tree =
      DefDef(mods, nme.CONSTRUCTOR.toString, Nil, paramss, Some(d.TypeTree()), rhs)
  }

  // qual.T[A, B](x, y)(z)
  object InitCall extends InitCallHelper {
    def apply(qual: Option[Tree], name: String, tparams: Seq[TypeTree], argss: Seq[Seq[Tree]]): Tree = {
      val select = if (qual.isEmpty) d.Ident(name.toTermName) else d.Select(qual.get, name.toTypeName)
      val fun = if (tparams.size == 0) select else TypeApply(select, tparams.toList)
      ApplySeq(fun, argss).withPos(enclosingPosition)
    }
  }

  object Param extends ParamHelper {
    def apply(mods: Seq[Tree], name: String, tpe: Option[TypeTree], default: Option[Tree]): Tree = {
      d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), getOrEmpty(default)).withMods(fromMods(mods)).withFlags(Flags.TermParam).withPos(enclosingPosition)
    }

    def unapply(tree: Tree): Option[(Seq[Tree], String, Option[TypeTree], Option[Tree])] = tree match {
      case valdef @ c.ValDef(name, tpe, _) =>
        val tpeOpt = if (tpe == d.TypeTree()) None else Some(tpe)
        val defaultOpt = if (valdef.rhs == d.EmptyTree) None else Some(valdef.rhs)
        Some((toMods(valdef.mods), name.toString, tpeOpt, defaultOpt))
      case _ => None
    }
  }

  object TypeParam extends TypeParamHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[TypeTree], tboundsOpt: Option[TypeTree], cbounds: Seq[TypeTree]): TypeTree = {
      val tbounds = tboundsOpt.getOrElse(d.TypeBoundsTree(d.EmptyTree, d.EmptyTree)).asInstanceOf[d.TypeBoundsTree]
      val inner =
        if (cbounds.size == 0) tbounds
        else d.ContextBounds(tbounds, cbounds.toList.map(d.AppliedTypeTree(_, d.Ident(name.toTypeName))))

      val body =
        if (tparams.size == 0) inner
        else d.PolyTypeTree(tparams.toList.asInstanceOf[List[d.TypeDef]], inner)

      d.TypeDef(name.toTypeName, body).withMods(fromMods(mods)).withPos(enclosingPosition)
    }
  }

  object Self extends SelfHelper {
    def apply(name: String, tpe: TypeTree): Tree =
      d.ValDef(name.toTermName, tpe, d.EmptyTree)

    def apply(name: String): Tree =
      d.ValDef(name.toTermName, d.TypeTree(), d.EmptyTree).withPos(enclosingPosition)

    def unapply(tree: Tree): Option[(String, Option[TypeTree])] = tree match {
      case c.ValDef(name, tp, _)  =>
        val tpOpt = if (tp == d.TypeTree()) None else Some(tp)
        Some((name.toString, tpOpt))
      case _ =>  None
    }
  }

  // types
  object TypeIdent extends TypeIdentHelper {
    def apply(name: String): TypeTree = d.Ident(name.toTypeName).withPos(enclosingPosition)
  }

  object TypeSelect extends TypeSelectHelper {
    def apply(qual: Tree, name: String): TypeTree = d.Select(qual, name.toTypeName)
  }

  object TypeSingleton extends TypeSingletonHelper {
    def apply(ref: Tree): TypeTree = d.SingletonTypeTree(ref)
  }

  object TypeApply extends TypeApplyHelper {
    def apply(tpe: TypeTree, args: Seq[TypeTree]): TypeTree = d.AppliedTypeTree(tpe, args.toList)
  }

  object TypeApplyInfix extends TypeApplyInfixHelper {
    def apply(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree = d.InfixOp(lhs, d.Ident(op.toTypeName), rhs)
  }

  object TypeFunction extends TypeFunctionHelper {
    def apply(params: Seq[TypeTree], res: TypeTree): TypeTree = d.Function(params.toList, res)
  }

  object TypeTuple extends TypeTupleHelper {
    def apply(args: Seq[TypeTree]): TypeTree = d.Tuple(args.toList).withPos(enclosingPosition)
  }

  object TypeAnd extends TypeAndHelper {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree = d.AndTypeTree(lhs, rhs)
  }

  object TypeOr extends TypeOrHelper {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree = d.OrTypeTree(lhs, rhs)
  }

  object TypeRefine extends TypeRefineHelper {
    def apply(tpe : Option[TypeTree], stats: Seq[Tree]): TypeTree =
      d.RefinedTypeTree(tpe.getOrElse(d.EmptyTree), stats.toList).withPos(enclosingPosition)
  }

  object TypeBounds extends TypeBoundsHelper {
    def apply(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree = {
      require(lo.nonEmpty || hi.nonEmpty)
      d.TypeBoundsTree(lo.getOrElse(d.EmptyTree), hi.getOrElse(d.EmptyTree))
    }
  }

  object TypeRepeated extends TypeRepeatedHelper {
    def apply(tpe: TypeTree): TypeTree = d.PostfixOp(tpe, d.Ident(nme.raw.STAR))
  }

  object TypeByName extends TypeByNameHelper {
    def apply(tpe: TypeTree): TypeTree = d.ByNameTypeTree(tpe)
  }

  object TypeAnnotated extends TypeAnnotatedHelper {
    def apply(tpe: TypeTree, annots: Seq[Tree]): TypeTree = {
      require(annots.size > 0)
      annots.tail.foldRight(d.Annotated(tpe, annots.head)) { (ann, acc) =>
        d.Annotated(acc, ann)
      }
    }
  }

  // terms
  object Lit extends LitHelper {
    def apply(value: Any): Tree = d.Literal(Constant(value)).withPos(enclosingPosition)
    def unapply(tree: Tree): Option[Any] = tree match {
      case c.Literal(Constant(v)) => Some(v)
      case _ => None
    }
  }

  object Ident extends IdentHelper {
    def apply(name: String): Tree = d.Ident(name.toTermName).withPos(enclosingPosition)
  }

  object Select extends SelectHelper {
    def apply(qual: Tree, name: String): Tree = d.Select(qual, name.toTermName)
  }

  object This extends ThisHelper {
    def apply(qual: String): Tree = d.This(d.Ident(qual.toTypeName)).withPos(enclosingPosition)
    def apply(qual: Tree): Tree = d.This(qual.asInstanceOf[d.Ident])
  }

  object Super extends SuperHelper {
    def apply(thisp: String, superp: String): Tree =
      d.Super(d.Ident(thisp.toTypeName), d.Ident(superp.toTypeName)).withPos(enclosingPosition)
  }

  object Interpolate extends InterpolateHelper {
    def apply(prefix: String, parts: Seq[String], args: Seq[Tree]): Tree = {
      val thickets =
        for { (arg, part) <- args.zip(parts.take(args.size)) }
          yield {
            val expr = arg match {
              case tree: d.Ident => tree
              case tree => d.Block(Nil, tree)
            }
            d.Thicket(Lit(part), expr)
          }
      val segments =
        if (parts.size > args.size)
          thickets :+ Lit(parts.last)
        else thickets

      d.InterpolatedString(prefix.toTermName, segments.toList).withPos(enclosingPosition)
    }
  }

  object Apply extends ApplyHelper {
    def apply(fun: Tree, args: Seq[Tree]): Tree = d.Apply(fun, args.toList)
    def unapply(tree: Tree): Option[(Tree, Seq[Tree])] = tree match {
      case c.Apply(fun, args) => Some((fun, args))
      case _ => None
    }
  }

  object ApplyType extends ApplyTypeHelper {
    def apply(fun: Tree, args: Seq[TypeTree]): Tree = d.TypeApply(fun, args.toList)

    def unapply(tree: Tree): Option[(Tree, Seq[TypeTree])] = tree match {
      case c.TypeApply(fun, args) => Some((fun, args))
      case _ => None
    }
  }

  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  object Infix extends InfixHelper {
    def apply(lhs: Tree, op: String, rhs: Tree): Tree =
      d.Apply(d.Select(lhs, op.toTermName), List(rhs))
  }

  object Prefix extends PrefixHelper {
    def apply(op: String, od: Tree): Tree = d.PrefixOp(d.Ident(op.toTermName), od)
  }

  object Postfix extends PostfixHelper {
    def apply(od: Tree, op: String): Tree = d.PostfixOp(od, d.Ident(op.toTermName))
  }

  object Assign extends AssignHelper {
    def apply(lhs: Tree, rhs: Tree): Tree = d.Assign(lhs, rhs)

    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case c.Assign(lhs, rhs) => Some((lhs, rhs))
      case _ => None
    }
  }

  object Return extends ReturnHelper {
    def apply(expr: Tree): Tree = d.Return(expr, d.EmptyTree)
  }

  object Throw extends ThrowHelper {
    def apply(expr: Tree): Tree = d.Throw(expr)
  }

  object Ascribe extends AscribeHelper {
    def apply(expr: Tree, tpe: Tree): Tree = d.Typed(expr, tpe)
  }

  object Annotated extends AnnotatedHelper {
    def apply(expr: Tree, annots: Seq[Tree]): Tree = {
      require(annots.size > 0)
      annots.tail.foldRight(d.Annotated(expr, annots.head)) { (ann, acc) =>
        d.Annotated(acc, ann)
      }
    }
  }

  object Tuple extends TupleHelper {
    def apply(args: Seq[Tree]): Tree = d.Tuple(args.toList).withPos(enclosingPosition)
  }

  object Block extends BlockHelper {
    def apply(stats: Seq[Tree]): Tree = {
      if (stats.size == 0)
        d.Block(stats.toList, d.EmptyTree).withPos(enclosingPosition)
      else
        d.Block(stats.init.toList, stats.last)
    }
  }

  object If extends IfHelper {
    def apply(cond: Tree, thenp: Tree, elsep: Option[Tree]): Tree =
      d.If(cond, thenp, elsep.getOrElse(d.EmptyTree))
  }

  object Match extends MatchHelper {
    def apply(expr: Tree, cases: Seq[Tree]): Tree =
      d.Match(expr, cases.toList.asInstanceOf[List[d.CaseDef]])
  }

  object Case extends CaseHelper {
    def apply(pat: Tree, cond: Option[Tree], body: Tree): Tree =
      d.CaseDef(pat, cond.getOrElse(d.EmptyTree), body)
  }

  object Try extends TryHelper {
    def apply(expr: Tree, cases: Seq[Tree], finallyp: Option[Tree]): Tree =
      d.Try(expr, cases.toList.asInstanceOf[List[d.CaseDef]], finallyp.getOrElse(d.EmptyTree))

    def apply(expr: Tree, handler: Tree, finallyp: Option[Tree]): Tree =
      d.ParsedTry(expr, handler, finallyp.getOrElse(d.EmptyTree))
  }

  object Function extends FunctionHelper {
    def apply(params: Seq[Tree], body: Tree): Tree =
      d.Function(params.toList, body)
  }

  object PartialFunction extends PartialFunctionHelper {
    def apply(cases: Seq[Tree]): Tree =
      d.Match(d.Thicket(Nil), cases.toList.asInstanceOf[List[d.CaseDef]]).withPos(enclosingPosition)
  }

  object While extends WhileHelper {
    def apply(expr: Tree, body: Tree): Tree = d.WhileDo(expr, body)
  }

  object DoWhile extends DoWhileHelper {
    def apply(body: Tree, expr: Tree): Tree = d.DoWhile(body, expr)
  }

  object For extends ForHelper {
    def apply(enums: Seq[Tree], body: Tree): Tree = ???
  }

  object GenFrom extends GenFromHelper {
    def apply(pat: Tree, rhs: Tree): Tree = ???
  }

  object GenAlias extends GenAliasHelper {
    def apply(pat: Tree, rhs: Tree): Tree = ???
  }

  object Guard extends GuardHelper {
    def apply(cond: Tree): Tree = ???
  }

  object Yield extends YieldHelper {
    def apply(expr: Tree): Tree = ???
  }

  // can be InitCall or AnonymClass
  object New extends NewHelper {
    def apply(tpe: Tree): Tree = d.New(tpe)
  }

  object Named extends NamedHelper {
    def apply(name: String, expr: Tree): Tree =
      d.NamedArg(name.toTermName, expr)
  }

  object Repeated extends RepeatedHelper {
    def apply(expr: Tree): Tree =
      d.Typed(expr, d.Ident(tpnme.WILDCARD_STAR))
  }

  // patterns
  object Bind extends BindHelper {
    def apply(name: String, expr: Tree): Tree =
      d.Bind(name.toTermName, expr)
  }

  object Alternative extends AlternativeHelper {
    def apply(lhs: Tree, rhs: Tree): Tree =
      d.Alternative(List(lhs, rhs))
  }

  // importees
  object Import extends ImportHelper {
    def apply(items: Seq[Tree]): Tree =
      if (items.size == 1)
        items(0).withPos(enclosingPosition)
      else
        d.Thicket(items.toList).withPos(enclosingPosition)
  }

  object ImportItem extends ImportItemHelper {
    def apply(ref: Tree, importees: Seq[Tree]): Tree =
      d.Import(ref, importees.toList)
  }

  object ImportName extends ImportNameHelper {
    def apply(name: String): Tree = d.Ident(name.toTermName).withPos(enclosingPosition)
  }

  object ImportRename extends ImportRenameHelper {
    def apply(from: String, to: String): Tree =
      d.Thicket(d.Ident(from.toTermName), d.Ident(to.toTermName)).withPos(enclosingPosition)
  }

  object ImportHide extends ImportHideHelper {
    def apply(name: String): Tree =
      d.Thicket(d.Ident(name.toTermName), d.Ident(nme.WILDCARD)).withPos(enclosingPosition)
  }

  // modifiers
  object Mod extends ModHelper {

    object Private extends PrivateHelper {
      // Dummy trees to retrofit Dotty AST
      private case class PrivateTree(within: String) extends d.Tree

      def apply(within: String): Tree = PrivateTree(within)
      def unapply(tree: Tree): Option[String] = tree match {
        case PrivateTree(within) => Some(within)
        case _                   => None
      }
    }

    object Protected extends ProtectedHelper {
      // Dummy trees to retrofit Dotty AST
      private case class ProtectedTree(within: String) extends d.Tree

      def apply(within: String): Tree = ProtectedTree(within)
      def unapply(tree: Tree): Option[String] = tree match {
        case ProtectedTree(within) => Some(within)
        case _                     => None
      }
    }

    object Val extends ValHelper {
      // Dummy trees to retrofit Dotty AST
      private case class ValTree() extends d.Tree

      def apply(): Tree = ValTree()
      def unapply(tree: Tree): Boolean = tree.isInstanceOf[ValTree]
    }

    object Var extends VarHelper {
      // Dummy trees to retrofit Dotty AST
      private case class VarTree() extends d.Tree

      def apply(): Tree = VarTree()
      def unapply(tree: Tree): Boolean = tree.isInstanceOf[VarTree]
    }

    object Implicit extends ImplicitHelper {
      // Dummy trees to retrofit Dotty AST
      private case class ImplicitTree() extends d.Tree

      def apply(): Tree = ImplicitTree()
      def unapply(tree: Tree): Boolean = tree.isInstanceOf[ImplicitTree]
    }

    object Final extends FinalHelper {
      // Dummy trees to retrofit Dotty AST
      private case class FinalTree() extends d.Tree

      def apply(): Tree = FinalTree()
      def unapply(tree: Tree): Boolean = tree.isInstanceOf[FinalTree]
    }

    object Sealed extends SealedHelper {
      // Dummy trees to retrofit Dotty AST
      private case class SealedTree() extends d.Tree

      def apply(): Tree = SealedTree()
      def unapply(tree: Tree): Boolean = tree.isInstanceOf[SealedTree]
    }

    object Override extends OverrideHelper {
      // Dummy trees to retrofit Dotty AST
      private case class OverrideTree() extends d.Tree

      def apply(): Tree = OverrideTree()
      def unapply(tree: Tree): Boolean = tree.isInstanceOf[OverrideTree]
    }

    object Abstract extends AbstractHelper {
      // Dummy trees to retrofit Dotty AST
      private case class AbstractTree() extends d.Tree

      def apply(): Tree = AbstractTree()
      def unapply(tree: Tree): Boolean = tree.isInstanceOf[AbstractTree]
    }

    object Lazy extends LazyHelper {
      // Dummy trees to retrofit Dotty AST
      private case class LazyTree() extends d.Tree

      def apply(): Tree = LazyTree()
      def unapply(tree: Tree): Boolean = tree.isInstanceOf[LazyTree]
    }

    object Inline extends InlineHelper {
      // Dummy trees to retrofit Dotty AST
      private case class InlineTree() extends d.Tree

      def apply(): Tree = InlineTree()
      def unapply(tree: Tree): Boolean = tree.isInstanceOf[InlineTree]
    }

    object Type extends TypeHelper {
      // Dummy trees to retrofit Dotty AST
      private case class TypeTree() extends d.Tree

      def apply(): Tree = TypeTree()
      def unapply(tree: Tree): Boolean = tree.isInstanceOf[TypeTree]
    }

    object Case extends CaseHelper {
      // Dummy trees to retrofit Dotty AST
      private case class CaseTree() extends d.Tree

      def apply(): Tree = CaseTree()
      def unapply(tree: Tree): Boolean = tree.isInstanceOf[CaseTree]
    }

    object Contravariant extends ContravariantHelper {
      // Dummy trees to retrofit Dotty AST
      private case class ContravariantTree() extends d.Tree

      def apply(): Tree = ContravariantTree()
      def unapply(tree: Tree): Boolean = tree.isInstanceOf[ContravariantTree]
    }

    object Covariant extends CovariantHelper {
      // Dummy trees to retrofit Dotty AST
      private case class CovariantTree() extends d.Tree

      def apply(): Tree = CovariantTree()
      def unapply(tree: Tree): Boolean = tree.isInstanceOf[CovariantTree]
    }

    object Annot extends AnnotHelper {
      // Dummy trees to retrofit Dotty AST
      private case class AnnotTree(body: Tree) extends d.Tree

      def apply(body: Tree): Tree = AnnotTree(body)
      def unapply(tree: Tree): Option[Tree] = tree match {
        case AnnotTree(body) => Some(body)
        case _               => None
      }
    }
  }
}
