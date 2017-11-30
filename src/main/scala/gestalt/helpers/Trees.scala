package scala.gestalt
package helpers

import api._

trait Trees {
  def root: TermTree = api.Ident("_root_")
  def empty: TermTree = api.Ident("_empty_")

  def Ident(tp: TermRef)(implicit c: Dummy): tpd.Tree = api.Ident.apply(Denotation.symbol(Type.denot(tp).get))

  implicit class UntypedTreeOps(tree: Tree) {
    def pos: Pos = Pos.pos(tree)

    def show: String = untpd.show(tree)

    def traverse(pf: PartialFunction[Tree, Unit]): Unit =
      untpd.traverse(tree)(pf)

    def exists(pf: PartialFunction[Tree, Boolean]): Boolean =
      untpd.exists(tree)(pf)

    def transform(pf: PartialFunction[Tree, Tree]): Tree =
      untpd.transform(tree)(pf)
  }

  implicit class UntypedTermTreeOps(tree: TermTree) {
    def appliedTo(args: TermTree*): TermTree = Apply(tree, args.toList)
    def selectType(path: String): TypeTree = {
      val parts = path.split('.')

      val prefix = parts.init.foldLeft[TermTree](tree) { (prefix, name) =>
        Select(prefix, name)
      }

      TypeSelect(prefix, parts.last)
    }

    def select(path: String): TermTree = {
      val parts = path.split('.')
      parts.foldLeft[TermTree](tree) { (prefix, name) =>
        Select(prefix, name)
      }
    }
  }

   /**--------------------- Apply helpers ---------------------------------*/
  def ApplySeq(fun: TermTree, argss: List[List[TermTree]]): TermTree =
    argss.foldLeft(fun) { (acc, args) => Apply(acc, args) }

  def ApplySeq(fun: tpd.Tree, argss: List[List[tpd.Tree]])(implicit c: Dummy): tpd.Tree =
    argss.foldLeft(fun) { (acc, args) => Apply(acc, args) }

  object ApplySeq {
    def unapply(call: tpd.Tree): Option[(tpd.Tree, List[List[tpd.Tree]])] = {
      def recur(acc: List[List[tpd.Tree]], term: tpd.Tree): (tpd.Tree, List[List[tpd.Tree]]) = term match {
        case api.Apply(fun, args) => recur(args +: acc, fun) // inner-most is in the front
        case fun => (fun, acc)
      }

      Some(recur(Nil, call))
    }
  }

  /**--------------------- typed tree ---------------------------------*/
  private def tpdOps: api.tpd.type  =    api.toolbox.tpd.asInstanceOf[api.tpd.type]

  implicit class TypedTreeOps(tree: tpd.Tree) {
    def pos: Pos = Pos.pos(tree)
    def tpe: Type = tpdOps.typeOf(tree)
    def show: String = tpdOps.show(tree)
    def wrap: Splice = TypedSplice(tree)
    def subst(from: List[Symbol], to: List[Symbol]): tpd.Tree = tpdOps.subst(tree)(from, to)
    def isDef: Boolean = tpdOps.isDef(tree)

    def select(name: String): tpd.Tree = Select(tree, name)
    def appliedTo(args: List[tpd.Tree]): tpd.Tree = Apply(tree, args.toList)
    def appliedToTypes(args: List[Type]): tpd.Tree = ApplyType(tree, args.toList)

    def traverse(pf: PartialFunction[tpd.Tree, Unit]): Unit =
      tpdOps.traverse(tree)(pf)

    def exists(pf: PartialFunction[tpd.Tree, Boolean]): Boolean =
      tpdOps.exists(tree)(pf)

    def transform(pf: PartialFunction[tpd.Tree, tpd.Tree]): tpd.Tree =
      tpdOps.transform(tree)(pf)
  }

  implicit class TpdDefTreeOps(tree: tpd.DefTree) {
    def symbol: Symbol = tpdOps.symbol(tree)
  }

  implicit class TpdRefTreeOps(tree: tpd.RefTree) {
    def symbol: Symbol = tpdOps.symbol(tree)
  }

  implicit def tpd2untpd(tree: tpd.Tree): Splice = TypedSplice(tree)
  implicit def tpd2untpdList(trees: List[tpd.Tree]): List[TermTree] = trees.map(TypedSplice.apply)
  implicit def tpd2untpdListList(treess: List[List[tpd.Tree]]): List[List[TermTree]] = treess.map(_.map(TypedSplice.apply))

}