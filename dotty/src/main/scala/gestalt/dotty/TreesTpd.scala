package scala.gestalt.dotty

import gestalt.core
import dotty.tools.dotc
import dotc.core.{ Types => DTypes, _ }
import dotc.ast.{ tpd => t, untpd => d }
import dotc.typer.ProtoTypes._
import dotc.typer.ConstFold
import StdNames._
import NameOps._
import Contexts.Context
import Constants.Constant
import Decorators._

import scala.collection.mutable.Set


class Tpd(val toolbox: Toolbox) extends core.Tpd {
  import toolbox.symbols.Symbol
  import toolbox.types.Type
  import toolbox.enclosingPosition
  import toolbox.Position

  implicit val ctx: Context = toolbox.ctx

  type Tree    = t.Tree
  type DefTree = t.Tree
  type RefTree = t.Tree

  implicit class TreeHelper(tree: t.Tree) {
    def withPosition: t.Tree = tree.withPos(enclosingPosition)
  }

  // new qual.T[A, B](x, y)(z)
  object NewInstance extends NewInstanceImpl {
    def apply(tp: Type, argss: List[List[Tree]]): Tree = argss match {
      case head :: tail =>
        tail.foldLeft[Tree](t.New(tp, head).withPosition) { (acc, args) => Apply(acc, args) }
      case Nil =>
        t.New(tp).withPosition
    }
  }

  // terms

  object Return extends ReturnImpl {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case t.Return(expr, _) => Some(expr)
      case _ => None
    }
  }

  object If extends IfImpl {
    def apply(cond: Tree, thenp: Tree, elsep: Tree): Tree =
      t.If(cond, thenp, elsep)

    def unapply(tree: Tree): Option[(Tree, Tree, Tree)] = tree match {
      case tree: t.If => Some(tree.cond, tree.thenp, tree.elsep)
      case _          => None
    }
  }

  object Function extends FunctionImpl {
    def apply(params: List[Type], resTp: Type)(bodyFn: List[RefTree] => Tree): Tree = {
      val meth = ctx.newSymbol(
        ctx.owner, nme.ANON_FUN,
        Flags.Synthetic | Flags.Method,
        DTypes.MethodType(params.map(p => NameKinds.UniqueName.fresh("param".toTermName)), params, resTp)
      )
      t.Closure(meth, paramss => {
        ensureOwner(bodyFn(paramss.head), meth)
      })
    }

    def apply(params: List[Symbol], body: Tree): Tree = {
      apply(params.map(_.info), body.tpe) { args =>
        subst(body)(params, args.map(_.symbol))
      }
    }

    def unapply(tree: Tree): Option[(List[Symbol], Tree)] = tree match {
      case t.Block(Nil, body) => unapply(body)
      case t.Block((meth : t.DefDef) :: Nil, _ : t.Closure) if meth.name == nme.ANON_FUN =>
        Some((meth.vparamss.head.map(_.symbol), meth.rhs))
      case _ => None
    }
  }

  object While extends WhileImpl {
    // { <label> def while$(): Unit = if (cond) { body; while$() } ; while$() }
    def apply(cond: Tree, body: Tree): Tree = {
      val sym = ctx.newSymbol(ctx.owner, nme.WHILE_PREFIX, Flags.Label | Flags.Synthetic,
        DTypes.MethodType(Nil, ctx.definitions.UnitType), coord = cond.pos)

      val body2 = ensureOwner(body, sym)
      val call = t.Apply(t.ref(sym), Nil)
      val rhs = t.If(cond, t.Block(body2 :: Nil, call), t.Literal(Constant(())))
      t.Block(List(t.DefDef(sym, rhs)), call)
    }

    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case t.Block((ddef : t.DefDef) :: Nil, t.Apply(f, Nil))
      if f.symbol.name == nme.WHILE_PREFIX =>
        ddef.rhs match {
          case t.If(cond, t.Block(body :: Nil, _), _) =>
            Some((cond, body))
          case t.If(cond, t.Block(body :+ expr, _), _) =>
            Some((cond, t.Block(body, expr)) )
        }
      case _ => None
    }
  }

  object DoWhile extends DoWhileImpl {
    // { label def doWhile$(): Unit = { body; if (cond) doWhile$() } ; doWhile$() }
    def apply(body: Tree, cond: Tree): Tree = {
      val sym = ctx.newSymbol(ctx.owner, nme.DO_WHILE_PREFIX, Flags.Label | Flags.Synthetic,
        DTypes.MethodType(Nil, ctx.definitions.UnitType), coord = cond.pos)

      val body2 = ensureOwner(body, sym)
      val call = t.Apply(t.ref(sym).withPosition, Nil).withPosition
      val rhs = t.Block(body2 :: Nil, t.If(cond, call, t.Literal(Constant(())).withPosition))
      t.Block(List(t.DefDef(sym, rhs)), call)
    }

    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case t.Block((ddef : t.DefDef) :: Nil, t.Apply(f, Nil))
      if f.symbol.name == nme.DO_WHILE_PREFIX =>
        val t.Block(body :: Nil, t.If(cond, _, _)) = ddef.rhs
        Some((body, cond))
      case _ => None
    }
  }

  object Lit extends LitImpl {
    def apply(value: Any): Tree = t.Literal(Constant(value)).withPosition

    def unapply(tree: Tree): Option[Any] = tree match {
      case t.Literal(Constant(v)) => Some(v)
      case _ => None
    }
  }

  object Ident extends IdentImpl {
    def apply(symbol: Symbol): RefTree = t.ref(symbol).withPosition

    def unapply(tree: Tree): Option[Symbol] = tree match {
      case id: t.Ident if id.name.isTermName => Some(id.symbol)
      case _ => None
    }
  }

  object Select extends SelectImpl {
    def apply(qual: Tree, name: String): RefTree =
      t.Select(qual, name.toTermName)

    def unapply(tree: Tree): Option[(Tree, Symbol)] = tree match {
      case t.Select(qual, _) => Some((qual, tree.symbol))
      case _ => None
    }
  }

  object Apply extends ApplyImpl {
    def apply(fun: Tree, args: List[Tree]): Tree = typedApply(fun, args)

    def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
      case t.Apply(fun, args) => Some((fun, args))
      case _ => None
    }

    def typedApply(fun: Tree, args: List[Tree])(implicit ctx: Context): Tree = {
      val tree = d.Apply(fun, args)
      val typr = ctx.typer

      val proto = new FunProtoTyped(args, DTypes.WildcardType, typr)
      val fun1 = typr.adapt(fun, proto)

      /** Type application where arguments come from prototype, and no implicits are inserted */
      def simpleApply(fun1: Tree, proto: FunProtoTyped)(implicit ctx: Context): Tree =
        t.methPart(fun1).tpe match {
          case funRef: DTypes.TermRef =>
            val app = new typr.ApplyToTyped(tree, fun1, funRef, args, DTypes.WildcardType)
            typr.convertNewGenericArray(ConstFold(app.result))
          case _ =>
            throw new Error(s"unexpected type.\n fun = $fun,\n methPart(fun) = ${t.methPart(fun)},\n methPart(fun).tpe = ${t.methPart(fun).tpe},\n tpe = ${fun.tpe}")
        }

      fun1.tpe match {
        case err: DTypes.ErrorType => d.cpy.Apply(tree)(fun1, args).withType(err)
        case _ =>
          if (proto.isDropped) fun1
          else
            typr.tryEither {
              implicit ctx => simpleApply(fun1, proto)
            } {
              (failedVal, failedState) => failedState.commit(); failedVal
            }
      }

    }

  }

  object Ascribe extends AscribeImpl {
    def apply(expr: Tree, tpe: Type): Tree =
      t.Typed(expr, t.TypeTree(tpe)).withPosition

    def unapply(tree: Tree): Option[(Tree, Type)] = tree match {
      case t.Typed(expr, tpt) => Some((expr, tpt.tpe))
      case _ => None
    }
  }

  object Assign extends AssignImpl {
    def apply(lhs: Tree, rhs: Tree): Tree =
      t.Assign(lhs, rhs)

    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case t.Assign(lhs, rhs) => Some((lhs, rhs))
      case _ => None
    }
  }

  object Block extends BlockImpl {
    def apply(stats: List[Tree], expr: Tree): Tree =
      t.Block(stats, expr)

    def unapply(tree: Tree): Option[(List[Tree], Tree)] = tree match {
      case block: t.Block => Some((block.stats, block.expr))
      case _ => None
    }
  }

  object Match extends MatchImpl {
    def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
      case t.Match(expr, cases) => Some((expr, cases))
      case _ => None
    }
  }

  object Case extends CaseImpl {
    def unapply(tree: Tree): Option[(Tree, Option[Tree], Tree)] = tree match {
      case t.CaseDef(pat, cond, body) =>
        val condOpt = if (cond == t.EmptyTree) None else Some(cond)
        Some((pat, condOpt, body))
      case _ => None
    }
  }

  object SeqLiteral extends SeqLiteralImpl {
    def apply(trees: List[Tree], tp: Type): Tree = {
      val tpSeq = ctx.definitions.RepeatedParamType.appliedTo(tp)
      t.Typed(t.SeqLiteral(trees, t.TypeTree(tp)), t.TypeTree(tpSeq))
    }

    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case t.Typed(t.SeqLiteral(elems,_), _) => Some(elems)
      case _ => None
    }
  }

  object ApplyType extends ApplyTypeImpl {
    def apply(fun: Tree, args: List[Type]): Tree = {
      val typr = ctx.typer
      val proto = new PolyProto(args, DTypes.WildcardType)
      val fun1 = typr.adapt(fun, proto)

      t.TypeApply(fun1, args.map(arg => t.TypeTree(arg)))
    }

    def unapply(tree: Tree): Option[(Tree, List[Type])] = tree match {
      case t.TypeApply(fun, targs) => Some((fun, targs.map(_.tpe)))
      case _ => None
    }
  }

  object ValDef extends ValDefImpl {
    def apply(rhs: Tree, tpOpt: Option[Type], mutable: Boolean): DefTree = {
      val flags = if (mutable) Flags.Mutable else Flags.EmptyFlags
      val vsym = ctx.newSymbol(ctx.owner, toolbox.fresh("temp").toTermName, flags, tpOpt.getOrElse(rhs.tpe))
      // also add flags to tree, so that `degrade` works
      t.ValDef(vsym, rhs).withFlags(flags).asInstanceOf[DefTree]
    }

    def apply(sym: Symbol, rhs: Tree): DefTree = {
      t.ValDef(sym.asTerm, rhs).withFlags(sym.flags).asInstanceOf[DefTree]
    }

    def unapply(tree: Tree): Option[(Symbol, Tree)] = tree match {
      case vdef : t.ValDef =>
        Some((vdef.symbol, vdef.rhs))
      case _ => None
    }
  }

  /*------------------------------- TreeOps-------------------------------------*/
  def pos(tree: Tree): Position = tree.pos

  def show(tree: Tree): String = tree.show

  def symbol(tree: Tree): Symbol = tree.symbol

  def isDef(tree: Tree): Boolean = tree.isDef

  def traverse(tree: Tree)(pf: PartialFunction[Tree, Unit]): Unit =
    new t.TreeTraverser {
      override def traverse(tree: Tree)(implicit ctx: Context) =
        pf.lift(tree).getOrElse(super.traverseChildren(tree))
    }.traverse(tree)

  def exists(tree: Tree)(pf: PartialFunction[Tree, Boolean]): Boolean = {
    var r = false
    traverse(tree) {
      case t if pf.isDefinedAt(t) && !r => r = pf(t)
    }
    r
  }

  def transform(tree: Tree)(pf: PartialFunction[Tree, Tree]): Tree = {
    new t.TreeMap() {
      override def transform(tree: Tree)(implicit ctx: Context) = {
        def updateOwner(subtree: Tree) = ensureOwner(subtree, ctx.owner)
        pf.lift(tree).map(updateOwner(_)).getOrElse(super.transform(tree))
      }
    }.transform(tree)
  }

  /** subst symbols in tree */
  def subst(tree: Tree)(from: List[Symbol], to: List[Symbol]): Tree = new t.TreeOps(tree).subst(from, to)

  /** type associated with the tree */
  def typeOf(tree: Tree): Type = tree.tpe

  /** wrap a typed tree as an untyped tree */
  def splice(tree: Tree): d.Tree = {
    val treeNew = toolbox.tpd.ensureOwner(tree, ctx.owner)

    d.TypedSplice(treeNew)(ctx)
  }

  def degrade(tree: Tree)(pf: PartialFunction[Tree, d.Tree]): d.Tree = new d.TreeMap() {
    def erase(tree: d.Tree)(implicit ctx: Context): d.Tree = tree match {
      case t.Typed(tree, _: t.TypeTree) =>
        this.transform(tree)
      case tree: t.TypeTree =>
        d.TypedSplice(tree)
      case tree: t.Inlined =>
        this.transform(tree.call)
      case t.Literal(Constant(v)) =>
        d.Literal(Constant(v))
      case vdef: t.ValDef =>
        d.ValDef(vdef.name, transform(vdef.tpt), transform(vdef.rhs)).withFlags(vdef.symbol.flags &~ Flags.Touched)
      case t.UnApply(t.Select(extractor, _), _, pats) =>
        d.Apply(erase(extractor), pats.map(erase))
      case t.UnApply(t.TypeApply(t.Select(extractor, _), _), _, pats) =>
        d.Apply(erase(extractor), pats.map(erase))
      case t.Apply(fun, SeqLiteral(args) :: Nil) =>
        d.Apply(transform(fun), args.map(transform))
      // case dtree: t.MemberDef =>
      //   dtree.withFlags(dtree.symbol.flags &~ Flags.Touched)
      // case t.Ident(name) =>
      //   d.Ident(name)
      case While(cond, body) =>
        d.WhileDo(this.transform(cond), this.transform(body))
      case DoWhile(body, cond) =>
        d.DoWhile(this.transform(body), this.transform(cond))
      case _ =>
        super.transform(tree)
    }

    override def transform(tree: d.Tree)(implicit ctx: Context) =
      pf.lift(tree.asInstanceOf[t.Tree]).getOrElse(erase(tree))
  }.transform(tree)

  /*------------------------------- helpers -------------------------------------*/

  def ensureOwner(tree: Tree, owner: Symbol): Tree = {
    val froms = getOwners(tree)
    froms.foldRight(tree) { (from, acc) =>
      if (from eq owner) acc
      else new t.TreeOps(acc).changeOwner(from, owner)
    }
  }

  def getOwners(tree: Tree): List[Symbol] = {
    val owners = Set.empty[Symbol]
    new t.TreeTraverser {
      def traverse(tree: t.Tree)(implicit ctx: Context): Unit = tree match {
        case tree: t.DefTree if tree.symbol.exists =>
          owners += tree.symbol.owner
        case _ =>
          traverseChildren(tree)
      }
    }.traverse(tree)

    owners.toList
  }
}
