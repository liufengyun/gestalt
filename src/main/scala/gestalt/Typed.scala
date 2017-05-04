package scala.gestalt
package typed

import scala.gestalt.{ Trees => UntpdTrees }

trait Trees extends Params with TypeParams with ValDefs with {

  val untpd: UntpdTrees
  val symbols: Symbols

  type Tree     >: Null <: AnyRef
  type TypeTree >: Null <: Tree
  type TermTree >: Null <: Tree
  type DefTree  >: Null <: Tree

  type ValDef    <: DefTree
  type InitCall  <: Tree

  val Throw: ThrowImpl
  trait ThrowImpl {
    def unapply(tree: Tree): Option[TermTree]
  }

  val If: IfImpl
  trait IfImpl {
    def unapply(tree: Tree): Option[(TermTree, TermTree, TermTree)]
  }

  val Try: TryImpl
  trait TryImpl {
    def unapply(tree: Tree): Option[Tree, Seq[Tree], finallyp: TermTree]
  }

  val New: NewImpl
  trait NewImpl {
    // can only be InitCall,  AnonymClass not supported
    def unapply(tree: Tree): Option[Tree]
  }

  val Lit: LitImpl
  trait LitImpl {
    def apply(value: Any): TermTree
    def unapply(tree: Tree): Option[Any]
  }

  val Apply: ApplyImpl
  trait ApplyImpl {
    def unapply(tree: Tree): Option[(TermTree, Seq[TermTree])]
  }

  val ApplyType: ApplyTypeImpl
  trait ApplyTypeImpl {
    def unapply(tree: Tree): Option[(TermTree, Seq[TypeTree])]
  }

  val Ident: IdentImpl
  trait IdentImpl {
    def unapply(tree: Tree): Option[String]
  }

  val This: ThisImpl
  trait ThisImpl {
    def unapply(tree: Tree): Option[String]
  }

  val Select: SelectImpl
  trait SelectImpl {
    def unapply(tree: Tree): Option[(TermTree, String)]
  }

  val Ascribe: AscribeImpl
  trait AscribeImpl {
    def unapply(tree: Tree): Option[(TermTree, TypeTree)]
  }

  val Assign: AssignImpl
  trait AssignImpl {
    def unapply(tree: Tree): Option[(TermTree, TermTree)]
  }

  val Return: ReturnImpl
  trait ReturnImpl {
    def unapply(tree: Tree): Option[TermTree]
  }

  val Block: BlockImpl
  trait BlockImpl {
    def unapply(tree: Tree): Option[Seq[Tree]]
  }

  val Match: MatchImpl
  trait MatchImpl {
    def unapply(tree: Tree): Option[(TermTree, Seq[Tree])]
  }

  val Case: CaseImpl
  trait CaseImpl {
    def unapply(tree: Tree): Option[(TermTree, Option[TermTree], TermTree)]
  }

  val Tuple: TupleImpl
  trait TupleImpl {
    def unapply(tree: Tree): Option[Seq[TermTree]]
  }

  val SeqLiteral: SeqLiteralImpl
  trait SeqLiteralImpl {
    def unapply(tree: Tree): Option[Seq[TermTree]]
  }

  object ApplySeq {
    def unapply(call: TermTree): Option[(Tree, Seq[Seq[TermTree]])] = {
      def recur(acc: Seq[Seq[TermTree]], term: TermTree): (TermTree, Seq[Seq[TermTree]]) = term match {
        case Apply(fun, args) => recur(args +: acc, fun) // inner-most is in the front
        case fun => (fun, acc)
      }
      Some(recur(Nil, call))
    }
  }

  // traverser
  def traverse(tre: Tree)(pf: PartialFunction[Tree, Unit]): Unit
  def exists(tree: Tree)(pf: PartialFunction[Tree, Boolean]): Boolean
  def transform(tree: Tree)(pf: PartialFunction[Tree, Tree]): Tree
}

trait ValDefs { this: Trees =>
  implicit class ValDefOps(tree: ValDef) {
    def symbol: symbols.Symbol = ValDef.symbol(tree)
    def name: String = ValDef.name(tree)
    def tptOpt: Option[TypeTree] = ValDef.tptOpt(tree)
    def rhs: TermTree = ValDef.rhs(tree)
    def copy(rhs: untpd.TermTree): untpd.ValDef = ValDef.copyRhs(tree)(rhs)
  }

  val ValDef: ValDefImpl
  trait ValDefImpl {
    def symbol(tree: ValDef): symbols.Symbol
    def name(tree: ValDef): String
    def rhs(tree: ValDef): TermTree
    def tptOpt(tree: ValDef): Option[TypeTree]
    def copyRhs(tree: ValDef)(rhs: untpd.TermTree): untpd.ValDef
    def get(tree: Tree): Option[ValDef]
    def unapply(tree: Tree): Option[(String, Option[TypeTree], TermTree)]
  }

  object OfValDef {
    def unapply(tree: Tree): Option[ValDef] = ValDef.get(tree)
  }
}
