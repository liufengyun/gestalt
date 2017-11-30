package scala.gestalt.dotty

import scala.gestalt.core.{Location, Toolbox => Tbox}
import dotty.tools.dotc._
import core._
import ast.{Trees => c, tpd => t, untpd => d}
import StdNames._
import NameOps._
import Names.TermName
import Decorators._
import Constants._
import d.modsDeco
import typer.ProtoTypes._
import util.Positions.Position

import scala.collection.mutable.Set


class Toolbox(enclosingPosition: Position)(implicit ctx: Contexts.Context) extends Tbox {

  type Tree = d.Tree
  type TypeTree = d.Tree
  type TermTree = d.Tree
  type DefTree = d.Tree
  type PatTree = d.Tree

  type Mods = DottyModifiers

  type Splice = d.Tree
  type Ident  = d.Ident
  type Lit    = d.Literal

  type Param = d.ValDef
  type TypeParam = d.TypeDef
  type Class = d.TypeDef
  type Trait = d.TypeDef
  type Object = d.ModuleDef
  type ValDef = d.ValDef
  type ValDecl = d.ValDef
  type DefDef = d.DefDef
  type DefDecl = d.DefDef
  type Self = d.ValDef
  type InitCall = d.Tree

  type Context = Contexts.Context

  /*------------------------------ positions ------------------------------*/
  type Pos = Position

  object Pos extends PosImpl {
    def pos(tree: Tree): Pos = tree.pos

    def pos(tree: tpd.Tree)(implicit c: Dummy): Pos = tree.pos
  }

  /*------------------------------ modifiers ------------------------------*/
  case class DottyModifiers(dottyMods: d.Modifiers) extends Modifiers {
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
    def setPrivate(within: String): Mods =
      if (within == "this") DottyModifiers(dottyMods | Flags.Private | Flags.Local)
      else DottyModifiers(dottyMods.withPrivateWithin(within.toTypeName) | Flags.Private)

    def setProtected(within: String): Mods =
      if (within == "this") DottyModifiers(dottyMods | Flags.Protected | Flags.Local)
      else DottyModifiers(dottyMods.withPrivateWithin(within.toTypeName) | Flags.Protected)

    def setOverride: Mods = DottyModifiers(dottyMods | Flags.Override)
    def setFinal: Mods = DottyModifiers(dottyMods | Flags.Final)
    def setImplicit: Mods = DottyModifiers(dottyMods | Flags.Implicit)
    def setLazy: Mods = DottyModifiers(dottyMods | Flags.Lazy)
    def setSealed: Mods = DottyModifiers(dottyMods | Flags.Sealed)
    def setAbstract: Mods = DottyModifiers(dottyMods | Flags.Abstract)
    def setCase: Mods = DottyModifiers(dottyMods | Flags.Case)
    def setContravariant: Mods = DottyModifiers(dottyMods | Flags.Contravariant)
    def setCovariant: Mods = DottyModifiers(dottyMods | Flags.Covariant)
    def setInline: Mods = DottyModifiers(dottyMods | Flags.Inline)
    def setMutable: Mods = DottyModifiers(dottyMods | Flags.Mutable)

    def setValParam: Mods = DottyModifiers(dottyMods &~ Flags.Local &~ Flags.Mutable)
    def setVarParam: Mods = DottyModifiers(dottyMods &~ Flags.Local | Flags.Mutable)

    def withAddedAnnotation(annot: d.Tree): Mods = DottyModifiers(dottyMods.withAddedAnnotation(annot))
    def withAnnotations(annots: List[d.Tree]): Mods = DottyModifiers(dottyMods.withAnnotations(annots))

    def hasAnnotations: Boolean = dottyMods.hasAnnotations

    def annotations: List[Tree] = dottyMods.annotations

    def privateWithin: String =
      if (dottyMods.is(Flags.Local)) "this"
      else dottyMods.privateWithin.toString
  }

  // modifiers
  def emptyMods: Mods = DottyModifiers(d.EmptyModifiers)

  implicit  def toDotty(tbMods: Mods): d.Modifiers = tbMods.dottyMods

  implicit def fromDotty(dottyMods: d.Modifiers): Mods = DottyModifiers(dottyMods)

  /*------------------------------ diagnostics ------------------------------*/
  def fresh(prefix: String = "$local"): String = NameKinds.UniqueName.fresh(prefix.toTermName).toString

  // diagnostics - the implementation takes the position from the tree
  def error(message: String, pos: Pos): Unit = {
    ctx.error(message, pos)
  }

  /** stop macro transform - the implementation takes the position from the tree */
  def abort(message: String, pos: Pos): Nothing = {
    ctx.error(message, pos)
    throw new Exception(message)
  }

  /*------------------------------ helpers ------------------------------*/
  def getOrEmpty(treeOpt: Option[Tree]): Tree = treeOpt.getOrElse(d.EmptyTree)

  implicit class TreeHelper(tree: d.Tree) {
    def withPosition[T <: Tree] = tree.withPos(enclosingPosition).asInstanceOf[T]
  }

  def ApplySeq(fun: TermTree, argss: List[List[TermTree]]): TermTree =
    argss.foldLeft(fun) { (acc, args) => Apply(acc, args) }

  /*------------------------------ trees ------------------------------*/

  object NewAnonymClass extends NewAnonymClassImpl {
    def apply(parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Tree = {
      val init = d.DefDef(nme.CONSTRUCTOR, List(), List(), d.TypeTree(), d.EmptyTree)
      val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
      d.New(d.Template(init, parents, self, stats).withPosition)
    }

    def apply(parents: List[Type], stats: List[tpd.Tree]): tpd.Tree = {
      val owner = ctx.owner
      val parents1 =
        if (parents.head.classSymbol.is(Flags.Trait)) parents.head.parents.head :: parents
        else parents
      val cls = ctx.newNormalizedClassSymbol(owner, tpnme.ANON_FUN, Flags.Synthetic, parents1, coord = enclosingPosition)
      val constr = ctx.newConstructor(cls, Flags.Synthetic, Nil, Nil).entered

      val dummy = ctx.newLocalDummy(cls, enclosingPosition)

      val stats2 = stats.map { stat =>
        val owner = if (stat.isDef) cls else dummy
        val t.Block(tree :: Nil, _) = ensureOwner(t.Block(stat :: Nil, t.Literal(Constant(()))), owner)
        if (tree.isDef) tree.symbol.entered
        tree
      }

      val cdef: tpd.Tree = t.ClassDef(cls, t.DefDef(constr).withPosition, stats2).withPosition
      val newTree: tpd.Tree = t.New(cls.typeRef, Nil).withPosition
      val tdef: tpd.Tree = t.Block(cdef :: Nil, newTree).withPosition

      // fix bug in type assigner: it doesn't avoid type symbols
      tdef.withType(ctx.typeAssigner.avoid(newTree.tpe, t.localSyms(cdef :: Nil)))
    }
  }

  object TypeDecl extends TypeDeclImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], tboundsOpt: Option[TypeTree]): DefTree = {
      val tbounds = tboundsOpt.getOrElse(d.TypeBoundsTree(d.EmptyTree, d.EmptyTree))
      val body =
        if (tparams.size == 0) tbounds
        else d.LambdaTypeTree(tparams.asInstanceOf[List[d.TypeDef]], tbounds)
      d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
    }
  }

  object TypeAlias extends TypeAliasImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], rhs: TypeTree): DefTree = {
      val body =
        if (tparams.size == 0) rhs
        else d.LambdaTypeTree(tparams.asInstanceOf[List[d.TypeDef]], rhs)
      d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
    }
  }

  object PatDef extends PatDefImpl {
    def apply(mods: Mods, lhs: PatTree, tpe: Option[TypeTree], rhs: TermTree): DefTree =
      d.PatDef(mods, List(lhs), tpe.getOrElse(d.TypeTree()), rhs).withPosition
  }

  object SecondaryCtor extends SecondaryCtorImpl {
    def apply(mods: Mods, paramss: List[List[Param]], rhs: TermTree): DefTree =
      DefDef(mods, nme.CONSTRUCTOR.toString, Nil, paramss, Some(d.TypeTree()), rhs).withPosition
  }

  // qual.T[A, B](x, y)(z)
  object InitCall extends InitCallImpl {
    def apply(tpe: TypeTree, argss: List[List[TermTree]]): InitCall = {
      ApplySeq(tpe, argss).withPosition
    }
  }

  // new qual.T[A, B](x, y)(z)
  object NewInstance extends NewInstanceImpl {
    def apply(typeTree: TypeTree, argss: List[List[TermTree]]): TermTree = {
      ApplySeq(d.Select(d.New(typeTree), nme.CONSTRUCTOR), argss).withPosition
    }

    def apply(tp: Type, argss: List[List[tpd.Tree]])(implicit cap: Dummy): tpd.Tree = {
      argss match {
        case head :: tail => tail.foldLeft[tpd.Tree](t.New(tp, head)) { (acc, args) => Apply(acc, args) }
        case Nil => t.New(tp)
      }
    }
  }

  object Self extends SelfImpl {
    def apply(name: String, tpe: TypeTree): Self =
      d.ValDef(name.toTermName, tpe, d.EmptyTree).withPosition

    def apply(name: String): Self =
      d.ValDef(name.toTermName, d.TypeTree(), d.EmptyTree).withPosition
  }

  // types
  object TypeIdent extends TypeIdentImpl {
    def apply(name: String)(implicit c: Unsafe): TypeTree = d.Ident(name.toTypeName).withPosition
  }

  object TypeSelect extends TypeSelectImpl {
    def apply(qual: Tree, name: String): TypeTree = d.Select(qual, name.toTypeName).withPosition
  }

  object TypeSingleton extends TypeSingletonImpl {
    def apply(ref: Tree): TypeTree = d.SingletonTypeTree(ref).withPosition
  }

  object TypeApply extends TypeApplyImpl {
    def apply(tpe: TypeTree, args: List[TypeTree]): TypeTree = d.AppliedTypeTree(tpe, args).withPosition
  }

  object TypeInfix extends TypeInfixImpl {
    def apply(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree =
      d.InfixOp(lhs, d.Ident(op.toTypeName), rhs).withPosition
  }

  object TypeFunction extends TypeFunctionImpl {
    def apply(params: List[TypeTree], res: TypeTree): TypeTree =
      d.Function(params, res).withPosition
  }

  object TypeTuple extends TypeTupleImpl {
    def apply(args: List[TypeTree]): TypeTree = d.Tuple(args).withPosition
  }

  object TypeAnd extends TypeAndImpl {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree =
      d.AndTypeTree(lhs, rhs).withPosition
  }

  object TypeOr extends TypeOrImpl {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree =
      d.OrTypeTree(lhs, rhs).withPosition
  }

  object TypeRefine extends TypeRefineImpl {
    def apply(stats: List[Tree]): TypeTree =
      d.RefinedTypeTree(d.EmptyTree, stats).withPosition

    def apply(tpe: TypeTree, stats: List[Tree]): TypeTree =
      d.RefinedTypeTree(tpe, stats).withPosition
  }

  object TypeBounds extends TypeBoundsImpl {
    def apply(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree = {
      require(lo.nonEmpty || hi.nonEmpty)
      d.TypeBoundsTree(lo.getOrElse(d.EmptyTree), hi.getOrElse(d.EmptyTree)).withPosition
    }
  }

  object TypeRepeated extends TypeRepeatedImpl {
    def apply(tpe: TypeTree): TypeTree =
      d.PostfixOp(tpe, d.Ident(nme.raw.STAR)).withPosition
  }

  object TypeByName extends TypeByNameImpl {
    def apply(tpe: TypeTree): TypeTree =
      d.ByNameTypeTree(tpe).withPosition
  }

  object TypeAnnotated extends TypeAnnotatedImpl {
    def apply(tpe: TypeTree, annots: List[Tree]): TypeTree = {
      require(annots.size > 0)
      annots.tail.foldRight(d.Annotated(tpe, annots.head).withPosition) { (ann: Tree, acc: TypeTree) =>
        d.Annotated(acc, ann).withPosition[TypeTree]
      }
    }
  }

  // terms

  object Super extends SuperImpl {
    def apply(thisp: String, superp: String): TermTree =
      d.Super(d.Ident(thisp.toTypeName), d.Ident(superp.toTypeName)).withPosition
  }

  object Interpolate extends InterpolateImpl {
    def apply(prefix: String, parts: List[String], args: List[TermTree]): TermTree = {
      val thickets =
        for {(arg, part) <- args.zip(parts.take(args.size))}
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

      d.InterpolatedString(prefix.toTermName, segments).withPosition
    }
  }


  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  object Infix extends InfixImpl {
    def apply(lhs: TermTree, op: String, rhs: TermTree): TermTree =
      d.InfixOp(lhs, d.Ident(op.toTermName), rhs).withPosition
  }

  object Prefix extends PrefixImpl {
    def apply(op: String, od: TermTree): TermTree = d.PrefixOp(d.Ident(op.toTermName), od).withPosition
  }

  object Postfix extends PostfixImpl {
    def apply(od: TermTree, op: String): TermTree =
      d.PostfixOp(od, d.Ident(op.toTermName)).withPosition
  }

  object Return extends ReturnImpl {
    def apply(expr: TermTree): TermTree = d.Return(expr, d.EmptyTree).withPosition
    def apply(): TermTree = d.Return(d.EmptyTree, d.EmptyTree).withPosition

    def apply(from: Symbol, expr: tpd.Tree): tpd.Tree = t.Return(expr, t.ref(from))

    def unapply(tree: tpd.Tree): Option[tpd.Tree] = tree match {
      case t.Return(expr, _) => Some(expr)
      case _ => None
    }
  }

  object Throw extends ThrowImpl {
    def apply(expr: TermTree): TermTree = d.Throw(expr).withPosition
  }

  object If extends IfImpl {
    def apply(cond: TermTree, thenp: TermTree, elsep: Option[TermTree]): TermTree =
      d.If(cond, thenp, elsep.getOrElse(d.EmptyTree)).withPosition

    def apply(cond: tpd.Tree, thenp: tpd.Tree, elsep: tpd.Tree)(implicit c: Dummy): tpd.Tree =
      t.If(cond, thenp, elsep)

    def unapply(tree: tpd.Tree): Option[(tpd.Tree, tpd.Tree, tpd.Tree)] = tree match {
      case tree: t.If => Some(tree.cond, tree.thenp, tree.elsep)
      case _          => None
    }
  }

  object Try extends TryImpl {
    def apply(expr: TermTree, cases: List[Tree], finallyp: Option[TermTree]): TermTree =
      d.Try(expr, cases.asInstanceOf[List[d.CaseDef]], finallyp.getOrElse(d.EmptyTree)).withPosition

    def apply(expr: TermTree, handler: Tree, finallyp: Option[TermTree]): TermTree =
      d.ParsedTry(expr, handler, finallyp.getOrElse(d.EmptyTree)).withPosition
  }

  object Function extends FunctionImpl {
    def apply(params: List[Param], body: TermTree): TermTree =
      d.Function(params, body).withPosition

    def apply(params: List[Type], resTp: Type)(bodyFn: List[tpd.RefTree] => tpd.Tree): tpd.Tree = {
      val meth = ctx.newSymbol(
        ctx.owner, nme.ANON_FUN,
        Flags.Synthetic | Flags.Method,
        Types.MethodType(params.map(p => NameKinds.UniqueName.fresh("param".toTermName)), params, resTp)
      )
      t.Closure(meth, paramss => {
        ensureOwner(bodyFn(paramss.head), meth)
      })
    }

    def apply(params: List[Symbol], body: tpd.Tree)(implicit c: Dummy): tpd.Tree = {
      apply(params.map(_.info), body.tpe) { args =>
        tpd.subst(body)(params, args.map(_.symbol))
      }
    }

    def unapply(tree: tpd.Tree): Option[(List[Symbol], tpd.Tree)] = tree match {
      case c.Block(Nil, body) => unapply(body)
      case c.Block((meth : t.DefDef) :: Nil, _ : t.Closure) if meth.name == nme.ANON_FUN =>
        Some((meth.vparamss.head.map(_.symbol), meth.rhs))
      case _ => None
    }
  }

  object While extends WhileImpl {
    def apply(cond: TermTree, body: TermTree): TermTree = d.WhileDo(cond, body).withPosition

    // { <label> def while$(): Unit = if (cond) { body; while$() } ; while$() }
    def apply(cond: tpd.Tree, body: tpd.Tree)(implicit c: Dummy): tpd.Tree = {
      val sym = ctx.newSymbol(ctx.owner, nme.WHILE_PREFIX, Flags.Label | Flags.Synthetic,
        Types.MethodType(Nil, ctx.definitions.UnitType), coord = cond.pos)

      val body2 = ensureOwner(body, sym)
      val call = t.Apply(t.ref(sym), Nil)
      val rhs = t.If(cond, t.Block(body2 :: Nil, call), t.Literal(Constant(())))
      t.Block(List(t.DefDef(sym, rhs)), call)
    }

    def unapply(tree: tpd.Tree): Option[(tpd.Tree, tpd.Tree)] = tree match {
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
    def apply(body: TermTree, cond: TermTree): TermTree = d.DoWhile(body, cond).withPosition

    // { label def doWhile$(): Unit = { body; if (cond) doWhile$() } ; doWhile$() }
    def apply(body: tpd.Tree, cond: tpd.Tree)(implicit c: Dummy): tpd.Tree = {
      val sym = ctx.newSymbol(ctx.owner, nme.DO_WHILE_PREFIX, Flags.Label | Flags.Synthetic,
        Types.MethodType(Nil, ctx.definitions.UnitType), coord = cond.pos)

      val body2 = ensureOwner(body, sym)
      val call = t.Apply(t.ref(sym), Nil)
      val rhs = t.Block(body2 :: Nil, t.If(cond, call, t.Literal(Constant(()))))
      t.Block(List(t.DefDef(sym, rhs)), call)
    }

    def unapply(tree: tpd.Tree): Option[(tpd.Tree, tpd.Tree)] = tree match {
      case t.Block((ddef : t.DefDef) :: Nil, t.Apply(f, Nil))
      if f.symbol.name == nme.DO_WHILE_PREFIX =>
        val t.Block(body :: Nil, t.If(cond, _, _)) = ddef.rhs
        Some((body, cond))
      case _ => None
    }
  }

  object For extends ForImpl {
    def ForDo(enums: List[Tree], body: TermTree): TermTree = d.ForDo(enums, body)
    def ForYield(enums: List[Tree], body: TermTree): TermTree = d.ForYield(enums, body)
    def GenFrom(pat: PatTree, rhs: TermTree): Tree = d.GenFrom(pat, rhs)
    def GenAlias(pat: PatTree, rhs: TermTree): Tree = d.GenAlias(pat, rhs)
    def Guard(cond: TermTree): Tree = cond
  }

  object Named extends NamedImpl {
    def apply(name: String, expr: TermTree): TermTree =
      d.NamedArg(name.toTermName, expr).withPosition
  }

  object Repeated extends RepeatedImpl {
    def apply(expr: TermTree): TermTree =
      d.Typed(expr, d.Ident(tpnme.WILDCARD_STAR)).withPosition
  }

  // patterns
  object Pat extends PatImpl {
    def Bind(name: String, expr: PatTree): PatTree =
      d.Bind(name.toTermName, expr).withPosition

    def Alt(trees: List[PatTree]): PatTree =
      d.Alternative(trees).withPosition

    def Lit(v: Any): PatTree =
      d.Literal(Constant(v)).withPosition

    def Var(name: String): PatTree =
      d.Ident(name.toTermName).withPosition

    def Ascribe(name: String, tp: TypeTree): PatTree =
      d.Typed(d.Ident(name.toTermName), tp).withPosition

    def Unapply(fun: TermTree, args: List[PatTree]): PatTree =
      d.Apply(fun, args).withPosition

    def Infix(lhs: PatTree, op: String, rhs: PatTree): PatTree =
      d.InfixOp(lhs, d.Ident(op.toTermName), rhs).withPosition

    def Tuple(pats: List[PatTree]): PatTree =
      d.Tuple(pats)
  }

  // importees
  object Import extends ImportImpl {
    def apply(items: List[Tree]): Tree =
      if (items.size == 1)
        items.head.withPosition
      else
        d.Thicket(items).withPosition

    def Item(ref: Tree, importees: List[Tree]): Tree =
      d.Import(ref, importees).withPosition

    def Name(name: String): Tree = d.Ident(name.toTermName).withPosition

    def Rename(from: String, to: String): Tree =
      d.Thicket(d.Ident(from.toTermName), d.Ident(to.toTermName)).withPosition

    def Hide(name: String): Tree =
      d.Thicket(d.Ident(name.toTermName), d.Ident(nme.WILDCARD)).withPosition
  }

  // extractors
  object Lit extends LitImpl {
    def apply(value: Any): tpd.Tree = t.Literal(Constant(value))

    def unapply(tree: tpd.Tree): Option[Any] = tree match {
      case t.Literal(Constant(v)) => Some(v)
      case _ => None
    }
  }

  object Ident extends IdentImpl {
    def apply(name: String)(implicit c: Unsafe): Ident = d.Ident(name.toTermName).withPosition

    def apply(symbol: Symbol): tpd.RefTree = t.ref(symbol)
    def unapply(tree: tpd.Tree): Option[Symbol] = tree match {
      case id: t.Ident if id.name.isTermName => Some(id.symbol)
      case _ => None
    }
  }

  object This extends ThisImpl {
    def apply(qual: String): TermTree = d.This(d.Ident(qual.toTypeName)).withPosition
  }

  object Select extends SelectImpl {
    def apply(qual: TermTree, name: String): TermTree = d.Select(qual, name.toTermName).withPosition

    def apply(qual: tpd.Tree, name: String)(implicit c: Dummy): tpd.RefTree =
      t.Select(qual, name.toTermName)

    def unapply(tree: tpd.Tree): Option[(tpd.Tree, Symbol)] = tree match {
      case t.Select(qual, _) => Some((qual, tree.symbol))
      case _ => None
    }
  }

  object Apply extends ApplyImpl {
    def apply(fun: TermTree, args: List[TermTree]): TermTree = d.Apply(fun, args).withPosition

    def apply(fun: tpd.Tree, args: List[tpd.Tree])(implicit c: Dummy): tpd.Tree = typedApply(fun, args)

    def unapply(tree: tpd.Tree): Option[(tpd.Tree, List[tpd.Tree])] = tree match {
      case t.Apply(fun, Seq(t.Typed(t.SeqLiteral(args, _), _))) => Some((fun, args))
      case t.Apply(fun, args) => Some((fun, args))
      case _ => None
    }

    def typedApply(fun: tpd.Tree, args: List[tpd.Tree])(implicit ctx: Context): tpd.Tree = {
      val tree = d.Apply(fun, args)
      val typr = ctx.typer

      val proto = new FunProtoTyped(args, Types.WildcardType, typr)
      val fun1 = typr.adapt(fun, proto)

      /** Type application where arguments come from prototype, and no implicits are inserted */
      def simpleApply(fun1: tpd.Tree, proto: FunProtoTyped)(implicit ctx: Context): tpd.Tree =
        t.methPart(fun1).tpe match {
          case funRef: TermRef =>
            val app = new typr.ApplyToTyped(tree, fun1, funRef, args, Types.WildcardType)
            typr.convertNewGenericArray(typer.ConstFold(app.result))
          case _ =>
            throw new Error(i"unexpected type.\n fun = $fun,\n methPart(fun) = ${t.methPart(fun)},\n methPart(fun).tpe = ${t.methPart(fun).tpe},\n tpe = ${fun.tpe}")
        }

      fun1.tpe match {
        case err: Types.ErrorType => d.cpy.Apply(tree)(fun1, args).withType(err)
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
    def apply(expr: TermTree, tpe: TypeTree): TermTree = d.Typed(expr, tpe).withPosition

    def apply(expr: tpd.Tree, tpe: Type)(implicit c: Dummy): tpd.Tree =
      t.Typed(expr, t.TypeTree(tpe))

    def unapply(tree: tpd.Tree): Option[(tpd.Tree, Type)] = tree match {
      case t.Typed(expr, tpt) => Some((expr, tpt.tpe))
      case _ => None
    }
  }

  object Assign extends AssignImpl {
    def apply(lhs: TermTree, rhs: TermTree): TermTree = d.Assign(lhs, rhs).withPosition

    def apply(lhs: tpd.Tree, rhs: tpd.Tree)(implicit c: Dummy): tpd.Tree = t.Assign(lhs, rhs)

    def unapply(tree: tpd.Tree): Option[(tpd.Tree, tpd.Tree)] = tree match {
      case t.Assign(lhs, rhs) => Some((lhs, rhs))
      case _ => None
    }
  }

  object Update extends UpdateImpl {
    def apply(fun: TermTree, argss: List[List[TermTree]], rhs: TermTree): TermTree = {
      d.Assign(ApplySeq(fun, argss), rhs)
    }
  }


  object Annotated extends AnnotatedImpl {
    def apply(expr: TermTree, annots: List[Tree]): TermTree = {
      require(annots.size > 0)
      annots.tail.foldRight(d.Annotated(expr, annots.head).withPosition) { (ann: Tree, acc: TermTree) =>
        d.Annotated(acc, ann).withPosition[TermTree]
      }
    }
  }

  object Block extends BlockImpl {
    def apply(stats: List[Tree]): TermTree = {
      if (stats.size == 0)
        d.Block(stats, d.EmptyTree).withPosition
      else
        d.Block(stats.init, stats.last).withPosition
    }

    def apply(stats: List[tpd.Tree], expr: tpd.Tree): tpd.Tree =
      t.Block(stats, expr)

    def unapply(tree: tpd.Tree): Option[(List[tpd.Tree], tpd.Tree)] = tree match {
      case block: t.Block => Some((block.stats, block.expr))
      case _ => None
    }
  }

  object Match extends MatchImpl {
    def apply(expr: TermTree, cases: List[Tree]): TermTree =
      d.Match(expr, cases.asInstanceOf[List[d.CaseDef]]).withPosition

    def unapply(tree: tpd.Tree): Option[(tpd.Tree, List[tpd.Tree])] = tree match {
      case t.Match(expr, cases) => Some((expr, cases))
      case _ => None
    }
  }

  object Case extends CaseImpl {
    def apply(pat: PatTree, cond: Option[TermTree], body: TermTree): Tree =
      d.CaseDef(pat, cond.getOrElse(d.EmptyTree), body).withPosition

    def unapply(tree: tpd.Tree): Option[(tpd.Tree, Option[tpd.Tree], tpd.Tree)] = tree match {
      case t.CaseDef(pat, cond, body) =>
        val condOpt = if (cond == t.EmptyTree) None else Some(cond)
        Some((pat, condOpt, body))
      case _ => None
    }
  }

  object PartialFunction extends PartialFunctionImpl {
    def apply(cases: List[Tree]): TermTree =
      d.Match(d.EmptyTree, cases.asInstanceOf[List[d.CaseDef]]).withPosition
  }

  object Tuple extends TupleImpl {
    def apply(args: List[TermTree]): TermTree = d.Tuple(args).withPosition
  }


  object SeqLiteral extends SeqLiteralImpl {
    def apply(trees: List[tpd.Tree], tp: Type): tpd.Tree = {
      val tpSeq = ctx.definitions.RepeatedParamType.appliedTo(tp)
      t.Typed(t.SeqLiteral(trees, t.TypeTree(tp)), t.TypeTree(tpSeq))
    }

    def unapply(tree: tpd.Tree): Option[List[tpd.Tree]] = tree match {
      case c.Typed(c.SeqLiteral(elems,_), _) => Some(elems)
      case _ => None
    }
  }

  object ApplyType extends ApplyTypeImpl {
    def apply(fun: Tree, args: List[TypeTree]): TermTree = d.TypeApply(fun, args).withPosition

    def apply(fun: tpd.Tree, args: List[Type])(implicit c: Dummy): tpd.Tree = {
      val typr = ctx.typer
      val proto = new PolyProto(args, Types.WildcardType)
      val fun1 = typr.adapt(fun, proto)

      t.TypeApply(fun1, args.map(arg => t.TypeTree(arg)))
    }

    def unapply(tree: tpd.Tree): Option[(tpd.Tree, List[Type])] = tree match {
      case t.TypeApply(fun, targs) => Some((fun, targs.map(_.tpe)))
      case _ => None
    }
  }


  object Object extends ObjectImpl {
    def apply(mods: Mods, name: String, parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Object = {
      val constr = d.DefDef(nme.CONSTRUCTOR, Nil, Nil, d.TypeTree(), d.EmptyTree)
      val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
      val templ = d.Template(constr, parents, self, stats)
      d.ModuleDef(name.toTermName, templ).withMods(mods).withPosition
    }
  }

  object Class extends ClassImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], ctorMods: Mods, paramss: List[List[Param]], parents: List[InitCall], selfOpt: Option[Self], stats: List[Tree]): Class = {
      val constr =
        if (paramss.isEmpty) d.DefDef(nme.CONSTRUCTOR, Nil, Nil, d.TypeTree(), d.EmptyTree)
        else {
          d.DefDef(nme.CONSTRUCTOR, tparams, paramss, d.TypeTree(), d.EmptyTree).withMods(ctorMods)
        }

      val self = if (selfOpt.isEmpty) d.EmptyValDef else selfOpt.get.asInstanceOf[d.ValDef]
      val templ = d.Template(constr, parents, self, stats)
      d.TypeDef(name.toTypeName, templ).withMods(mods).withPosition
    }
  }

  object Trait extends TraitImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], ctorMods: Mods, paramss: List[List[Param]], parents: List[InitCall], self: Option[Self], stats: List[Tree]): Trait =
      Class(mods.dottyMods | Flags.Trait, name, tparams, ctorMods, paramss, parents, self, stats).withPosition
  }

   // accessors
  object Param extends ParamImpl {
    def apply(mods: Mods, name: String, tpe: Option[TypeTree], default: Option[TermTree]): Param = {
      d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), getOrEmpty(default))
        .withMods(mods.dottyMods | Flags.TermParam).withPosition
    }
  }

  object TypeParam extends TypeParamImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], tboundsOpt: Option[TypeTree], cbounds: List[TypeTree]): TypeParam = {
      val tbounds = tboundsOpt.getOrElse(d.TypeBoundsTree(d.EmptyTree, d.EmptyTree)).asInstanceOf[d.TypeBoundsTree]
      val inner =
        if (cbounds.size == 0) tbounds
        else d.ContextBounds(tbounds, cbounds.map(d.AppliedTypeTree(_, d.Ident(name.toTypeName))))

      val body =
        if (tparams.size == 0) inner
        else d.LambdaTypeTree(tparams, inner)

      d.TypeDef(name.toTypeName, body).withMods(mods).withPosition
    }
  }

  object ValDef extends ValDefImpl {
    def apply(mods: Mods, name: String, tpe: Option[TypeTree], rhs: TermTree): ValDef =
      d.ValDef(name.toTermName, tpe.getOrElse(d.TypeTree()), rhs).withMods(mods).withPosition

    def apply(rhs: tpd.Tree, tpOpt: Option[Type], mutable: Boolean): tpd.DefTree = {
      val flags = if (mutable) Flags.Mutable else Flags.EmptyFlags
      val vsym = ctx.newSymbol(ctx.owner, NameKinds.UniqueName.fresh("temp".toTermName), flags, tpOpt.getOrElse(rhs.tpe))
      t.ValDef(vsym, rhs)
    }

    def apply(sym: Symbol, rhs: tpd.Tree): tpd.DefTree = t.ValDef(sym.asTerm, rhs)

    def unapply(tree: tpd.Tree): Option[(Symbol, tpd.Tree)] = tree match {
      case vdef : t.ValDef =>
        Some((vdef.symbol, vdef.rhs))
      case _ => None
    }
  }

  object ValDecl extends ValDeclImpl {
    def apply(mods: Mods, name: String, tpe: TypeTree): ValDecl =
      d.ValDef(name.toTermName, tpe, d.EmptyTree).withMods(mods).withPosition
  }

  object DefDef extends DefDefImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], paramss: List[List[Param]], tpe: Option[TypeTree], rhs: TermTree): DefDef = {
      d.DefDef(name.toTermName, tparams, paramss, tpe.getOrElse(d.TypeTree()), rhs).withMods(mods).withPosition
    }

    def apply(name: String, tp: MethodType)(bodyFn: (Symbol, List[List[tpd.RefTree]]) => tpd.Tree): tpd.DefTree = {
      val meth = ctx.newSymbol(
        ctx.owner, name.toTermName,
        Flags.Synthetic | Flags.Method,
        tp
      )

      t.DefDef(meth, paramss => {
        ensureOwner(bodyFn(meth, paramss), meth)
      }).withPosition
    }
  }

  object DefDecl extends DefDeclImpl {
    def apply(mods: Mods, name: String, tparams: List[TypeParam], paramss: List[List[Param]], tpe: TypeTree): DefDecl = {
      d.DefDef(name.toTermName, tparams, paramss, tpe, d.EmptyTree).withMods(mods).withPosition
    }

    def get(tree: Tree): Option[DefDecl] = tree match {
      case ddef : d.DefDef if ddef.forceIfLazy.isEmpty => Some(ddef)
      case _ => None
    }

    def unapply(tree: Tree): Option[(Mods, String, List[TypeParam], List[List[Param]], TypeTree)] = tree match {
      case ddef : d.DefDef if ddef.forceIfLazy.isEmpty =>
        Some((mods(ddef), name(ddef), ddef.tparams, ddef.vparamss, ddef.tpt))
      case _ => None
    }

    def mods(tree: DefDecl): Mods = new modsDeco(tree).mods
    def name(tree: DefDecl): String = tree.name.show
    def tparams(tree: DefDecl): List[TypeParam] = tree.tparams
    def paramss(tree: DefDecl): List[List[Param]] = tree.vparamss
    def tpt(tree: DefDecl): TypeTree = tree.tpt
  }

  object TypedSplice extends TypedSpliceImpl {
    def apply(tree: tpd.Tree): Splice = {
      val owners = getOwners(tree)

      // make sure the spliced tree only has one owner
      val treeNew = if (owners.length > 1) ensureOwner(tree, owners.head) else tree

      val newCtx = if (owners.isEmpty) ctx else ctx.withOwner(owners.head)
      d.TypedSplice(treeNew)(newCtx)
    }

    def unapply(tree: Tree): Option[tpd.Tree] = tree match {
      case d.TypedSplice(tree) => Some(tree)
      case _ => None
    }
  }


  /*------------------------------- TreeOps-------------------------------------*/

  object untpd extends untpdImpl {
    def show(tree: Tree): String = tree.show

    def traverse(tree: Tree)(pf: PartialFunction[Tree, Unit]): Unit =
      new d.UntypedTreeMap() {
        override def transform(tree: Tree)(implicit ctx: Context) = {
          pf.lift(tree).getOrElse(super.transform(tree))
          tree
        }
      }.transform(tree)

    def exists(tree: Tree)(pf: PartialFunction[Tree, Boolean]): Boolean = {
      var r = false
      traverse(tree) {
        case t if pf.isDefinedAt(t) && !r => r = pf(t)
      }
      r
    }

    def transform(tree: Tree)(pf: PartialFunction[Tree, Tree]): Tree = {
      new d.UntypedTreeMap() {
        override def transform(tree: Tree)(implicit ctx: Context) =
          pf.lift(tree).getOrElse(super.transform(tree))
      }.transform(tree)
    }
  }


  object tpd extends tpdImpl {
    type Tree    = t.Tree
    type DefTree = t.Tree
    type RefTree = t.Tree

    def show(tree: Tree): String = tree.show

    def symbol(tree: Tree): Symbol = tree.symbol

    def isDef(tree: Tree): Boolean = tree.isDef

    def traverse(tree: tpd.Tree)(pf: PartialFunction[tpd.Tree, Unit]): Unit =
      new t.TreeTraverser {
        override def traverse(tree: tpd.Tree)(implicit ctx: Context) =
          pf.lift(tree).getOrElse(super.traverseChildren(tree))
      }.traverse(tree)

    def exists(tree: tpd.Tree)(pf: PartialFunction[tpd.Tree, Boolean]): Boolean = {
      var r = false
      traverse(tree) {
        case t if pf.isDefinedAt(t) && !r => r = pf(t)
      }
      r
    }

    def transform(tree: tpd.Tree)(pf: PartialFunction[tpd.Tree, tpd.Tree]): tpd.Tree = {
      new t.TreeMap() {
        override def transform(tree: tpd.Tree)(implicit ctx: Context) = {
          def updateOwner(subtree: tpd.Tree) = ensureOwner(subtree, ctx.owner)
          pf.lift(tree).map(updateOwner(_)).getOrElse(super.transform(tree))
        }
      }.transform(tree)
    }

    /** subst symbols in tree */
    def subst(tree: tpd.Tree)(from: List[Symbol], to: List[Symbol]): tpd.Tree = new t.TreeOps(tree).subst(from, to)

    /** type associated with the tree */
    def typeOf(tree: tpd.Tree): Type = tree.tpe
  }

  /*------------------------------- types -------------------------------------*/

  type Type       = Types.Type
  type ParamRef   = Types.ParamRef
  type TermRef    = Types.TermRef
  type TypeRef    = Types.TypeRef
  type MethodType = Types.MethodType
  type Symbol     = Symbols.Symbol

  /** get the location where the def macro is used */
  def location: Location = Location(ctx.compilationUnit.source.file.name, enclosingPosition.line(), enclosingPosition.column())

  object Type extends TypeImpl {

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
    def classSymbol(tp: Type): Option[Symbol] = tp.classSymbol match {
      case Symbols.NoSymbol => None
      case cls      => Some(cls)
    }

    /** val fields of a case class Type -- only the ones declared in primary constructor */
    def caseFields(tp: Type): List[Denotation] = {
      tp.memberDenots(
        Types.fieldFilter,
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
        Types.fieldFilter,
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
        Types.takeAllFilter,
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
        Types.takeAllFilter,
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
      case tp: Types.NamedType => Some(tp.denot)
      case tp: Types.TypeProxy => denot(tp.underlying)
      case _ => None
    }

    /** The type representing  T[U1, ..., Un] */
    def appliedTo(tp: Type, args: List[Type]): Type = new TypeApplications(tp).appliedTo(args)(ctx)

    def toTree(tp: Type): tpd.Tree = t.TypeTree(tp)

    /** Infer an implicit instance of the given type */
    def infer(tp: Type): Option[tpd.Tree] = {
      var hasError = false
      def implicitArgError(msg: String => String) = hasError = true

      val res = ctx.typer.inferImplicitArg(tp, implicitArgError, enclosingPosition)
      if (hasError) None
      else Some(res)
    }
  }

  object ByNameType extends ByNameTypeImpl {
    def unapply(tp: Type): Option[Type] = tp match {
      case tp: Types.ExprType => Some(tp.underlying)
      case _ => None
    }
  }

  object MethodType extends MethodTypeImpl {
    def paramInfos(tp: MethodType): List[Type] = tp.paramInfos
    def instantiate(tp: MethodType)(params: List[Type]): Type = {
      tp.instantiate(params)
    }
    def unapply(tp: Type): Option[MethodType] = tp match {
      case tp: Types.MethodType => Some(tp)
      case _ => None
    }

    def apply(paramNames: List[String])
             (paramInfosExp: List[ParamRef] => List[Type],
              resultTypeExp: List[ParamRef] => Type): MethodType = {
      val names: List[TermName] = paramNames.map(_.toTermName)
      Types.MethodType(names)(
        mt => paramInfosExp(mt.paramRefs),
        mt => resultTypeExp(mt.paramRefs)
      )
    }
  }

  /*------------------------------- symbols -------------------------------------*/
  object Symbol extends SymbolImpl {
    /** name of a member */
    def name(mem: Symbol): String = mem.showName

    /** type of a member with respect to a prefix */
    def asSeenFrom(mem: Symbol, prefix: Type): Type = mem.asSeenFrom(prefix).info

    def typeRef(sym: Symbol): TypeRef = sym.typeRef
    def termRef(sym: Symbol): TermRef = sym.termRef

    def isCase(sym: Symbol): Boolean = sym.is(Flags.Case)
    def isTrait(sym: Symbol): Boolean = sym.is(Flags.Trait)
    def isPrivate(sym: Symbol): Boolean = sym.is(Flags.Private)
    def isProtected(sym: Symbol): Boolean = sym.is(Flags.Protected)
    def isOverride(sym: Symbol): Boolean = sym.is(Flags.Override)
    def isFinal(sym: Symbol): Boolean = sym.is(Flags.Final)
    def isImplicit(sym: Symbol): Boolean = sym.is(Flags.Implicit)
    def isLazy(sym: Symbol): Boolean = sym.is(Flags.Lazy)
    def isSealed(sym: Symbol): Boolean = sym.is(Flags.Sealed)
    def isAbstract(sym: Symbol): Boolean = sym.is(Flags.Abstract)
    def isMutable(sym: Symbol): Boolean = sym.is(Flags.Mutable)
  }

  def ensureOwner(tree: tpd.Tree, owner: Symbol): tpd.Tree = {
    val froms = getOwners(tree)
    froms.foldRight(tree) { (from, acc) =>
      if (from eq owner) acc
      else new t.TreeOps(acc).changeOwner(from, owner)
      //new ast.TreeTypeMap(oldOwners = from :: Nil, newOwners = owner :: Nil).apply(tree)
      // new t.TreeOps(acc).changeOwner(from, owner)
    }
  }

  def getOwners(tree: tpd.Tree): List[Symbol] = {
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

  /*------------------------------- Denotations -------------------------------------*/
  type Denotation = Denotations.Denotation

  object Denotation extends DenotationImpl {
    def name(denot: Denotation): String = denot.symbol.name.show

    def info(denot: Denotation): Type = denot.info.dealias

    def symbol(denot: Denotation): Symbol = denot.symbol
  }
}
