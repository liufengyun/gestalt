package scala.gestalt.core

trait Tpd {
  val toolbox: Toolbox
  import toolbox._

  type Tree      >: Null <: AnyRef
  type DefTree   <: Tree
  type RefTree   <: Tree

  def pos(tree: Tree): Position

  /** type associated with the tree */
  def typeOf(tree: Tree): Type

  def show(tree: Tree): String

  def symbol(tree: Tree): Symbol

  def isDef(tree: Tree): Boolean

  /** subst symbols in tree */
  def subst(tree: Tree)(from: List[Symbol], to: List[Symbol]): Tree

  def traverse(tree: Tree)(pf: PartialFunction[Tree, Unit]): Unit
  def exists(tree: Tree)(pf: PartialFunction[Tree, Boolean]): Boolean
  def transform(tree: Tree)(pf: PartialFunction[Tree, Tree]): Tree

  // definition trees
  def NewAnonymClass: NewAnonymClassImpl
  trait NewAnonymClassImpl {
    def apply(parents: List[Type], stats: List[Tree]): Tree
  }

  def NewInstance: NewInstanceImpl
  trait NewInstanceImpl {
    def apply(tp: Type, argss: List[List[Tree]]): Tree
  }

  def If: IfImpl
  trait IfImpl {
    def apply(cond: Tree, thenp: Tree, elsep: Tree): Tree
    def unapply(tree: Tree): Option[(Tree, Tree, Tree)]
  }

  def Function: FunctionImpl
  trait FunctionImpl {
    def apply(params: List[Type], resTp: Type)(bodyFn: List[RefTree] => Tree): Tree
    def apply(params: List[Symbol], body: Tree): Tree
    def unapply(tree: Tree): Option[(List[Symbol], Tree)]
  }

  def While: WhileImpl
  trait WhileImpl {
    def apply(cond: Tree, body: Tree): Tree
    def unapply(tree: Tree): Option[(Tree, Tree)]
  }

  def DoWhile: DoWhileImpl
  trait DoWhileImpl {
    def apply(body: Tree, cond: Tree): Tree
    def unapply(tree: Tree): Option[(Tree, Tree)]
  }

  def Lit: LitImpl
  trait LitImpl {
    def apply(value: Any): Tree
    def unapply(tree: Tree): Option[Any]
  }

  def Apply: ApplyImpl
  trait ApplyImpl {
    def apply(fun: Tree, args: List[Tree]): Tree
    def unapply(tree: Tree): Option[(Tree, List[Tree])]
  }

  def ApplyType: ApplyTypeImpl
  trait ApplyTypeImpl {
    def apply(fun: Tree, args: List[Type]): Tree
    def unapply(tree: Tree): Option[(Tree, List[Type])]
  }

  def Ident: IdentImpl
  trait IdentImpl {
    def apply(symbol: Symbol): RefTree
    def unapply(tree: Tree): Option[Symbol]
  }

  def Select: SelectImpl
  trait SelectImpl {
    def apply(qual: Tree, name: String): RefTree
    def unapply(tree: Tree): Option[(Tree, Symbol)]
  }

  def Ascribe: AscribeImpl
  trait AscribeImpl {
    def apply(expr: Tree, tpe: Type): Tree
    def unapply(tree: Tree): Option[(Tree, Type)]
  }

  def Assign: AssignImpl
  trait AssignImpl {
    def apply(lhs: Tree, rhs: Tree): Tree
    def unapply(tree: Tree): Option[(Tree, Tree)]
  }

  def Return: ReturnImpl
  trait ReturnImpl {
    def apply(from: Symbol, expr: Tree): Tree
    def unapply(tree: Tree): Option[Tree]
  }

  def Block: BlockImpl
  trait BlockImpl {
    def apply(stats: List[Tree], expr: Tree): Tree
    def unapply(tree: Tree): Option[(List[Tree], Tree)]
  }

  def Match: MatchImpl
  trait MatchImpl {
    def unapply(tree: Tree): Option[(Tree, List[Tree])]
  }

  def Case: CaseImpl
  trait CaseImpl {
    def unapply(tree: tpd.Tree): Option[(tpd.Tree, Option[tpd.Tree], tpd.Tree)]
  }

  def SeqLiteral: SeqLiteralImpl
  trait SeqLiteralImpl {
    def apply(trees: List[Tree], tp: Type): Tree
    def unapply(tree: Tree): Option[List[Tree]]
  }

  // only for interpolate that are expressions (not patterns)
  def Interpolate: InterpolateImpl
  trait InterpolateImpl {
    def unapply(tree: Tree): Option[(List[String], List[Tree])]
  }

  def ValDef: ValDefImpl
  trait ValDefImpl {
    def apply(rhs: Tree, tpOpt: Option[Type] = None, mutable: Boolean = false): DefTree
    def apply(sym: Symbol, rhs: Tree): DefTree
    def unapply(tree: Tree): Option[(Symbol, Tree)]
  }

  def DefDef: DefDefImpl
  trait DefDefImpl {
    def apply(name: String, tp: Type)(body: (Symbol, List[List[RefTree]]) => Tree): DefTree
  }

}