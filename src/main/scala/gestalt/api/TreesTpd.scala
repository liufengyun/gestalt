package scala.gestalt.api

import scala.gestalt.{ toolbox => impl, _ }

object Tpd {
  type Tree      >: Null <: AnyRef
  type DefTree   <: Tree
  type RefTree   <: Tree

  /**-------------------- tree ops ---------------------*/

  def pos(tree: Tree): Position = !impl.tpd.pos(!tree)

  /** type associated with the tree */
  def typeOf(tree: Tree): Type = !impl.tpd.typeOf(!tree)

  def show(tree: Tree): String = impl.tpd.show(!tree)

  def symbol(tree: Tree): Symbol = !impl.tpd.symbol(!tree)

  def isDef(tree: Tree): Boolean = impl.tpd.isDef(!tree)

  /** subst symbols in tree */
  def subst(tree: Tree)(from: List[Symbol], to: List[Symbol]): Tree =
    !impl.tpd.subst(!tree)(!from, !to)

  /** wrap typed tree as untyped tree */
  def splice(tree: Tree): untpd.Splice =
    !impl.tpd.splice(!tree)

  /** transform typed tree to untyped treee */
  def degrade(tree: Tree)(pf: PartialFunction[Tree, untpd.Tree]): untpd.TermTree =
    !impl.tpd.degrade(!tree)(!pf)

  def traverse(tree: Tree)(pf: PartialFunction[Tree, Unit]): Unit =
    impl.tpd.traverse(!tree)(!pf)

  def exists(tree: Tree)(pf: PartialFunction[Tree, Boolean]): Boolean =
    impl.tpd.exists(!tree)(!pf)

  def transform(tree: Tree)(pf: PartialFunction[Tree, Tree]): Tree =
    !impl.tpd.transform(!tree)(!pf)

  /**-------------------- constructor & extractors ---------------------*/

  object NewInstance {
    def apply(tp: Type, argss: List[List[Tree]]): Tree =
      !impl.tpd.NewInstance(!tp, !argss)
  }

  object If {
    def apply(cond: Tree, thenp: Tree, elsep: Tree): Tree =
      !impl.tpd.If(!cond, !thenp, !elsep)

    def unapply(tree: Tree): Option[(Tree, Tree, Tree)] =
      !impl.tpd.If.unapply(!tree)
  }

  object Function {
    def apply(params: List[Type], resTp: Type)(bodyFn: List[RefTree] => Tree): Tree = {
      val tpd = impl.tpd
      !tpd.Function.apply(!params, resTp.asInstanceOf[tpd.toolbox.Type])(!bodyFn)
    }

    def apply(params: List[Symbol], body: Tree): Tree =
      !impl.tpd.Function.apply(!params, !body)

    def unapply(tree: Tree): Option[(List[Symbol], Tree)] =
      !impl.tpd.Function.unapply(!tree)
  }

  object While {
    def apply(cond: Tree, body: Tree): Tree =
      !impl.tpd.While(!cond, !body)

    def unapply(tree: Tree): Option[(Tree, Tree)] =
      !impl.tpd.While.unapply(!tree)
  }

  object DoWhile {
    def apply(body: Tree, cond: Tree): Tree =
      !impl.tpd.DoWhile(!body, !cond)

    def unapply(tree: Tree): Option[(Tree, Tree)] =
      !impl.tpd.DoWhile.unapply(!tree)
  }

  object Lit {
    def apply(value: Any): Tree =
      !impl.tpd.Lit(value)

    def unapply(tree: Tree): Option[Any] =
      !impl.tpd.Lit.unapply(!tree)
  }

  object Apply {
    def apply(fun: Tree, args: List[Tree]): Tree =
      !impl.tpd.Apply(!fun, !args)

    def unapply(tree: Tree): Option[(Tree, List[Tree])] =
      !impl.tpd.Apply.unapply(!tree)
  }

  object ApplyType {
    def apply(fun: Tree, args: List[Type]): Tree =
      !impl.tpd.ApplyType(!fun, !args)

    def unapply(tree: Tree): Option[(Tree, List[Type])] =
      !impl.tpd.ApplyType.unapply(!tree)
  }

  object Ident {
    def apply(symbol: Symbol): RefTree =
      !impl.tpd.Ident(!symbol)

    def apply(tp: Types.TermRef)(implicit d: core.Dummy): Tree =
      Ident(Denotations.symbol(Types.denot(tp).get))

    def unapply(tree: Tree): Option[Symbol] =
      !impl.tpd.Ident.unapply(!tree)
  }

  object Select {
    def apply(qual: Tree, name: String): RefTree =
      !impl.tpd.Select(!qual, name)

    def unapply(tree: Tree): Option[(Tree, Symbol)] =
      !impl.tpd.Select.unapply(!tree)
  }

  object Ascribe {
    def apply(expr: Tree, tpe: Type): Tree =
      !impl.tpd.Ascribe(!expr, !tpe)

    def unapply(tree: Tree): Option[(Tree, Type)] =
      !impl.tpd.Ascribe.unapply(!tree)
  }

  object Assign {
    def apply(lhs: Tree, rhs: Tree): Tree =
      !impl.tpd.Assign(!lhs, !rhs)

    def unapply(tree: Tree): Option[(Tree, Tree)] =
      !impl.tpd.Assign.unapply(!tree)
  }

  object Return {
    def unapply(tree: Tree): Option[Tree] =
      !impl.tpd.Return.unapply(!tree)
  }

  object Block {
    def apply(stats: List[Tree], expr: Tree): Tree =
      !impl.tpd.Block(!stats, !expr)

    def unapply(tree: Tree): Option[(List[Tree], Tree)] =
      !impl.tpd.Block.unapply(!tree)

  }

  object Match {
    def unapply(tree: Tree): Option[(Tree, List[Tree])] =
      !impl.tpd.Match.unapply(!tree)
  }

  object Case {
    def unapply(tree: Tree): Option[(Tree, Option[Tree], Tree)] =
      !impl.tpd.Case.unapply(!tree)
  }

  object SeqLiteral {
    def apply(trees: List[Tree], tp: Type): Tree =
      !impl.tpd.SeqLiteral(!trees, !tp)

    def unapply(tree: Tree): Option[List[Tree]] =
      !impl.tpd.SeqLiteral.unapply(!tree)
  }

  object ValDef {
    def apply(rhs: Tree, tpOpt: Option[Type] = None, mutable: Boolean = false): DefTree =
      !impl.tpd.ValDef.apply(!rhs, !tpOpt, mutable)

    def apply(sym: Symbol, rhs: Tree): DefTree =
      !impl.tpd.ValDef.apply(!sym, !rhs)

    def unapply(tree: Tree): Option[(Symbol, Tree)] =
      !impl.tpd.ValDef.unapply(!tree)
  }

  /**-------------------- helpers ---------------------*/

  def ApplySeq(fun: Tree, argss: List[List[Tree]]): Tree =
    argss.foldLeft(fun) { (acc, args) => Apply(acc, args) }

  object ApplySeq {
    def unapply(call: Tree): Option[(Tree, List[List[Tree]])] = {
      def recur(acc: List[List[Tree]], term: Tree): (Tree, List[List[Tree]]) = term match {
        case Tpd.Apply(fun, args) => recur(args +: acc, fun) // inner-most is in the front
        case fun => (fun, acc)
      }

      Some(recur(Nil, call))
    }
  }

}