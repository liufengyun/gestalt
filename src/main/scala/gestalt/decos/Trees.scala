package scala.gestalt
package decos

trait Trees {
  import untpd._

  implicit class UntypedTreeOps(tree: Tree) {
    def pos: Position = untpd.pos(tree)

    def show: String = untpd.show(tree)
  }

  implicit class UntypedTermTreeOps(tree: TermTree) {
    def appliedTo(args: TermTree*): TermTree = Apply(tree, args.toList)
    def selectType(path: String): TypeTree = {
      val parts = path.split('.')

      val prefix = parts.init.foldLeft[TermTree](tree) { (prefix, name) =>
        Select(prefix, name)
      }

      untpd.TypeSelect(prefix, parts.last)
    }

    def select(path: String): TermTree = {
      val parts = path.split('.')
      parts.foldLeft[TermTree](tree) { (prefix, name) =>
        Select(prefix, name)
      }
    }
  }

  /**--------------------- typed tree ---------------------------------*/
  implicit class TypedTreeOps(tree: tpd.Tree) {
    def pos: Position = tpd.pos(tree)
    def tpe: Type = tpd.typeOf(tree)
    def show: String = tpd.show(tree)
    def splice: untpd.TermTree = tpd.splice(tree)
    def subst(from: List[Symbol], to: List[Symbol]): tpd.Tree = tpd.subst(tree)(from, to)
    def isDef: Boolean = tpd.isDef(tree)

    def select(name: String): tpd.Tree = tpd.Select(tree, name)
    def appliedTo(args: List[tpd.Tree]): tpd.Tree = tpd.Apply(tree, args.toList)
    def appliedToTypes(args: List[Type]): tpd.Tree = tpd.ApplyType(tree, args.toList)

    def degrade(pf: PartialFunction[tpd.Tree, untpd.Tree]): untpd.TermTree =
      tpd.degrade(tree)(pf)

    def traverse(pf: PartialFunction[tpd.Tree, Unit]): Unit =
      tpd.traverse(tree)(pf)

    def exists(pf: PartialFunction[tpd.Tree, Boolean]): Boolean =
      tpd.exists(tree)(pf)

    def transform(pf: PartialFunction[tpd.Tree, tpd.Tree]): tpd.Tree =
      tpd.transform(tree)(pf)
  }

  implicit class TpdDefTreeOps(tree: tpd.DefTree) {
    def symbol: Symbol = tpd.symbol(tree)
  }

  implicit class TpdRefTreeOps(tree: tpd.RefTree) {
    def symbol: Symbol = tpd.symbol(tree)
  }

  implicit def tpd2untpd(tree: tpd.Tree): untpd.Splice =
    tpd.splice(tree)
  implicit def tpd2untpdList(trees: List[tpd.Tree]): List[untpd.Splice] =
    trees.map(tpd.splice)
  implicit def tpd2untpdListList(treess: List[List[tpd.Tree]]): List[List[untpd.Splice]] =
    treess.map(_.map(tpd.splice))
}
