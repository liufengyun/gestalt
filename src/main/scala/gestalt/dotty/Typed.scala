package scala.gestalt
package dotty

import dotty.tools.dotc._
import core._
import ast.{ untpd => d, Trees => c, tpd => t }

trait TypedTree extends typed.Trees {
  type Tree     = t.Tree
  type TypeTree = t.Tree
  type TermTree = t.Tree
  type DefTree  = t.Tree

  type ValDef   = t.ValDef
  type InitCall = t.Tree

  object Throw extends ThrowImpl {
    def unapply(tree: Tree): Option[TermTree] = ???
  }

  object If extends IfImpl {
    def unapply(tree: Tree): Option[(TermTree, TermTree, TermTree)] = tree match {
      case c.If(cond, thenp, elsep) =>
        Some(cond, thenp, elsep)
      case _ => None
    }
  }

  object Try extends TryImpl {
    def unapply(tree: Tree): Option[Tree, Seq[Tree], finallyp: TermTree] = ???
  }
}
