package scala.gestalt.dotty

import scala.gestalt.Toolbox

import dotty.tools.dotc._
import core._
import ast.{ untpd => d, Trees => c }
import StdNames._
import NameOps._
import Flags._
import Contexts._
import Decorators._
import Constants._


class DottyToolbox(implicit ctx: Context) extends Toolbox {
  type Tree = d.Tree
  type TypeTree = d.Tree
  type Type = Types.Type

  def =:=(tp1: Type, tp2: Type): Boolean = ???
  def <:<(tp1: Type, tp2: Type): Boolean = ???
  def typeOf(path: String): Type = ???

  def getOrEmpty(treeOpt: Option[Tree]): Tree = treeOpt.getOrElse(d.EmptyTree)

  object Object extends ObjectHelper {
    def apply(mods: Seq[Tree], name: String, parents: Seq[Tree], selfOpt: Option[Tree], stats: Option[Seq[Tree]]): Tree = {
      // TODO mods
      val constr = d.DefDef(nme.CONSTRUCTOR, Nil, Nil, d.TypeTree(), d.EmptyTree)
      val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
      val body = if (stats.isEmpty) Nil else stats.get
      val templ = d.Template(constr, parents.toList, self, body)
      d.ModuleDef(name.toTermName, templ)
    }

    def unapply(tree: Tree): Option[(Seq[Tree], String, Seq[Tree], Option[Tree], Option[Seq[Tree]])] = tree match {
      case obj @ d.ModuleDef(name, templ @ c.Template(constr, parents, self, body)) =>
        // TODO mods
        val selfOpt = if (self.name == nme.WILDCARD) None else Some(self)
        Some((Nil, name.toString, parents, selfOpt, Some(templ.body)))
      case _ => None
    }
  }

  object Class extends ClassHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], ctor: Option[Tree], parents: Seq[Tree], self: Option[Tree], stats: Option[Seq[Tree]]): Tree = ???
    def unapply(tree: Tree): Option[(Seq[Tree], String, Seq[Tree], Option[Tree], Seq[Tree], Option[Tree], Option[Seq[Tree]])] = ???
  }

  object AnonymClass extends AnonymClassHelper {
    def apply(parents: Seq[Tree], self: Option[Tree], stats: Option[Seq[Tree]]): Tree = ???
  }

  object Trait extends TraitHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], ctor: Option[Tree], parents: Seq[Tree], self: Option[Tree], stats: Option[Seq[Tree]]): Tree = ???
    def unapply(tree: Tree): Option[(Seq[Tree], String, Seq[Tree], Option[Tree], Seq[Tree], Option[Tree], Option[Seq[Tree]])] = ???
  }

  object Type extends TypeHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], rhs: TypeTree): Tree = ???
  }

  object DefDef extends DefDefHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], paramss: Seq[Seq[Tree]], tpe: Option[TypeTree], rhs: Option[Tree]): Tree = {
      // TODO mods
      val types = tparams.toList.asInstanceOf[List[d.TypeDef]]
      val params = paramss.map(_.toList).toList.asInstanceOf[List[List[d.ValDef]]]
      d.DefDef(name.toTermName, types, params, tpe.getOrElse(d.TypeTree()), getOrEmpty(rhs))
    }
  }

  object ValDef extends ValDefHelper {
    def apply(mods: Seq[Tree], name: String, tpe: Option[TypeTree], rhs: Option[Tree]): Tree = ???
    def apply(mods: Seq[Tree], lhs: Tree, tpe: Option[TypeTree], rhs: Option[Tree]): Tree = ???
    def apply(mods: Seq[Tree], lhs: Seq[Tree], tpe: Option[TypeTree], rhs: Option[Tree]): Tree = ???
  }

  object PrimaryCtor extends PrimaryCtorHelper {
    def apply(mods: Seq[Tree], paramss: Seq[Seq[Tree]]): Tree = ???
  }

  object SecondaryCtor extends SecondaryCtorHelper {
    def apply(mods: Seq[Tree], paramss: Seq[Seq[Tree]], rhs: Tree): Tree = ???
  }

  // qual.T[A, B](x, y)(z)
  object InitCall extends InitCallHelper {
    def apply(qual: Option[Tree], name: String, tparams: Seq[TypeTree], argss: Seq[Seq[Tree]]): Tree = ???
  }

  object Param extends ParamHelper {
    def apply(mods: Seq[Tree], name: String, tpe: Option[TypeTree], default: Option[Tree]): Tree = {
      // TODO mods
      d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), getOrEmpty(default)).withFlags(TermParam)
    }
  }

  object TypeParam extends TypeParamHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[TypeTree], tbounds: TypeTree, cbounds: Seq[TypeTree]): TypeTree = ???
  }

  object Self extends SelfHelper {
    def apply(name: String, tpe: Option[TypeTree]): Tree = ???
  }

  // types
  object TypeIdent extends TypeIdentHelper {
    def apply(name: String): TypeTree = d.Ident(name.toTypeName)
  }

  object TypeSelect extends TypeSelectHelper {
    def apply(qual: Tree, name: String): TypeTree = d.Select(qual, name.toTypeName)
  }

  object TypeSingleton extends TypeSingletonHelper {
    def apply(ref: Tree): TypeTree = ???
  }

  object TypeApply extends TypeApplyHelper {
    def apply(tpe: TypeTree, args: Seq[TypeTree]): TypeTree = ???
  }

  object TypeApplyInfix extends TypeApplyInfixHelper {
    def apply(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree = ???
  }

  object TypeFunction extends TypeFunctionHelper {
    def apply(params: Seq[TypeTree], res: TypeTree): TypeTree = ???
  }

  object TypeTuple extends TypeTupleHelper {
    def apply(args: Seq[TypeTree]): TypeTree = ???
  }

  object TypeAnd extends TypeAndHelper {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree = ???
  }

  object TypeOr extends TypeOrHelper {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree = ???
  }

  object TypeRefine extends TypeRefineHelper {
    def apply(tpe : Option[TypeTree], stats: Seq[Tree]): TypeTree = ???
  }

  object TypeWildcard extends TypeWildcardHelper {
    def apply(bounds: Tree): TypeTree = ???
  }

  object TypeBounds extends TypeBoundsHelper {
    def apply(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree = ???
  }

  object TypeRepeated extends TypeRepeatedHelper {
    def apply(tpe: TypeTree): TypeTree = ???
  }

  object TypeByName extends TypeByNameHelper {
    def apply(tpe: TypeTree): TypeTree = ???
  }

  object TypeAnnotated extends TypeAnnotatedHelper {
    def apply(tpe: TypeTree, annots: Seq[Tree]): TypeTree = ???
  }

  // terms
  object Lit extends LitHelper {
    def apply(value: Any): Tree = d.Literal(Constant(value))
    def unapply(tree: Tree): Option[Any] = tree match {
      case c.Literal(Constant(v)) => Some(v)
      case _ => None
    }
  }

  object Wildcard extends WildcardHelper {
    def apply(): Tree = ???
  }

  object Ident extends IdentHelper {
    def apply(name: String): Tree = d.Ident(name.toTermName)
  }

  object Select extends SelectHelper {
    def apply(qual: Tree, name: String): Tree = d.Select(qual, name.toTermName)
  }

  object This extends ThisHelper {
    def apply(qual: String): Tree = ???
  }

  object Super extends SuperHelper {
    def apply(thisp: String, superp: String): Tree = ???
  }

  object Interpolate extends InterpolateHelper {
    def apply(prefix: String, parts: Seq[String], args: Seq[Tree]): Tree = ???
  }

  object Apply extends ApplyHelper {
    def apply(fun: Tree, args: Seq[Tree]): Tree = d.Apply(fun, args.toList)
    def unapply(tree: Tree): Option[(Tree, Seq[Tree])] = ???
  }

  object ApplyType extends ApplyTypeHelper {
    def apply(fun: Tree, args: Seq[TypeTree]): Tree = ???
  }

  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  object Infix extends InfixHelper {
    def apply(lhs: Tree, op: String, rhs: Tree): Tree = d.Apply(d.Select(lhs, op.toTermName), List(rhs))
  }

  object Prefix extends PrefixHelper {
    def apply(op: String, od: Tree): Tree = ???
  }

  object Postfix extends PostfixHelper {
    def apply(od: Tree, op: String): Tree = ???
  }

  object Assign extends AssignHelper {
    def apply(lhs: Tree, rhs: Tree): Tree = ???
  }

  object Return extends ReturnHelper {
    def apply(expr: Tree): Tree = ???
  }

  object Throw extends ThrowHelper {
    def apply(expr: Tree): Tree = ???
  }

  object Ascribe extends AscribeHelper {
    def apply(expr: Tree, tpe: Tree): Tree = ???
  }

  object Annotated extends AnnotatedHelper {
    def apply(expr: Tree, annots: Seq[Tree]): Tree = ???
  }

  object Tuple extends TupleHelper {
    def apply(args: Seq[Tree]): Tree = ???
  }

  object Block extends BlockHelper {
    def apply(stats: Seq[Tree]): Tree = {
      if (stats.size == 0)
        d.Block(stats.toList, d.EmptyTree)
      else
        d.Block(stats.init.toList, stats.last)
    }
  }

  object If extends IfHelper {
    def apply(cond: Tree, thenp: Tree, elsep: Option[Tree]): Tree = ???
  }

  object Match extends MatchHelper {
    def apply(expr: Tree, cases: Seq[Tree]): Tree = ???
  }

  object Case extends CaseHelper {
    def apply(pat: Tree, cond: Option[Tree], body: Tree): Tree = ???
  }

  object Try extends TryHelper {
    def apply(expr: Tree, cases: Seq[Tree], finallyp: Option[Tree]): Tree = ???
    def apply(expr: Tree, catchp: Tree, finallyp: Option[Tree]): Tree = ???
  }

  object Function extends FunctionHelper {
    def apply(params: Seq[Tree], body: Tree): Tree = ???
  }

  object PartialFunction extends PartialFunctionHelper {
    def apply(cases: Seq[Tree]): Tree = ???
  }

  object While extends WhileHelper {
    def apply(expr: Tree, body: Tree): Tree = ???
  }

  object DoWhile extends DoWhileHelper {
    def apply(body: Tree, expr: Tree): Tree = ???
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
    def apply(tpe: Tree): Tree = ???
  }

  object Named extends NamedHelper {
    def apply(name: String, expr: Tree): Tree = ???
  }

  object Repeated extends RepeatedHelper {
    def apply(expr: Tree): Tree = ???
  }

  // patterns
  object Bind extends BindHelper {
    def apply(name: String, expr: Tree): Tree = ???
  }

  object Alternative extends AlternativeHelper {
    def apply(lhs: Tree, rhs: Tree): Tree = ???
  }

  // importees
  object Import extends ImportHelper {
    def apply(items: Seq[Tree]): Tree = ???
  }

  object ImportItem extends ImportItemHelper {
    def apply(ref: Tree, importees: Seq[Tree]): Tree = ???
  }

  object ImportName extends ImportNameHelper {
    def apply(name: String): Tree = ???
  }

  object ImportRename extends ImportRenameHelper {
    def apply(from: String, to: String): Tree = ???
  }

  object ImportHide extends ImportHideHelper {
    def apply(name: String): Tree = ???
  }

  // modifiers
  object Mod extends ModHelper {
    object Private extends PrivateHelper {
      def apply(within: Tree): Tree = ???
    }

    object Protected extends ProtectedHelper {
      def apply(within: Tree): Tree = ???
    }

    object Val extends ValHelper {
      def apply(): Tree = ???
    }

    object Var extends VarHelper {
      def apply(): Tree = ???
    }

    object Implicit extends ImplicitHelper {
      def apply(): Tree = ???
    }

    object Final extends FinalHelper {
      def apply(): Tree = ???
    }

    object Sealed extends SealedHelper {
      def apply(): Tree = ???
    }

    object Override extends OverrideHelper {
      def apply(): Tree = ???
    }

    object Abstract extends AbstractHelper {
      def apply(): Tree = ???
    }

    object Lazy extends LazyHelper {
      def apply(): Tree = ???
    }

    object Inline extends InlineHelper {
      def apply(): Tree = ???
    }

    object Type extends TypeHelper {
      def apply(): Tree = ???
    }

    object Case extends CaseHelper {
      def apply(): Tree = ???
    }

    object Contravariant extends ContravariantHelper {
      def apply(): Tree = ???
    }

    object Covariant extends CovariantHelper {
      def apply(): Tree = ???
    }

    object Annot extends AnnotHelper{
      def apply(body: Tree): Tree = ???
    }
  }
}
