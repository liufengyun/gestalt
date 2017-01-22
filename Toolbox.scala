package scala.meta
package gestalt

trait Toolbox { t =>
  // portable trees -- minimum assumptions
  type Tree <: { def tpe: Type } // TODO: structural types performance penalty.
  type TypeTree                  // avoid errors in mixing type and term -- implementation can have TypeTree = Tree
  type Type

  // type operations
  implicit class TypeHelper(val tp1: Type) {
    def =:=(tp2: Type) = t.=:=(tp1, tp2)
    def <:<(tp2: Type) = t.<:<(tp1, tp2)
  }

  def =:=(tp1: Type, tp2: Type): Boolean
  def <:<(tp1: Type, tp2: Type): Boolean
  def typeOf(path: String): Type

  // standard constructors and extractors
  object Object {
    def apply(mods: List[Tree], name: String, parents: List[Tree], self: Option[Tree], stats: List[Tree]): Tree = ???
    def unapply(tree: Tree): Option[(List[Tree], String, List[Tree], Option[Tree], List[Tree])] = ???
  }

  object Class {
    def apply(mods: List[Tree], name: String, tparams: List[Tree], ctor: Option[Tree], parents: List[Tree], self: Option[Tree], stats: List[Tree]): Tree = ???
    def unapply(tree: Tree): Option[(List[Tree], String, List[Tree], Option[Tree], List[Tree], Option[Tree], List[Tree])] = ???
  }

  object AnonymClass {
    def apply(parents: List[Tree], self: Option[Tree], stats: List[Tree]): Tree = ???
  }

  object Trait {
    def apply(mods: List[Tree], name: String, tparams: List[Tree], paramss: List[List[Tree]], parents: List[Tree], self: Option[Tree], stats: List[Tree]): Tree = ???
    def unapply(tree: Tree): Option[(List[Tree], String, List[Tree], List[List[Tree]], List[Tree], Option[Tree], List[Tree])] = ???
  }

  object Type {
    def apply(mods: List[Tree], name: String, tparams: List[Tree], rhs: TypeTree): Tree = ???
  }

  object DefDef {
    def apply(mods: List[Tree], name: String, tparams: List[Tree], paramss: List[List[Tree]], tpe: Option[TypeTree], rhs: Option[Tree]): Tree = ???
  }

  object ValDef {
    def apply(mods: List[Tree], name: String, tpe: Option[TypeTree], rhs: Option[Tree]): Tree = ???
    def apply(mods: List[Tree], lhs: Tree, tpe: Option[TypeTree], rhs: Option[Tree]): Tree = ???
    def apply(mods: List[Tree], lhs: List[Tree], tpe: Option[TypeTree], rhs: Option[Tree]): Tree = ???
  }

  object PrimaryCtor {
    def apply(mods: List[Tree], paramss: List[List[Tree]]): Tree = ???
  }

  object SecondaryCtor {
    def apply(mods: List[Tree], paramss: List[List[Tree]], rhs: Tree): Tree = ???
  }

  // qual.T[A, B](x, y)(z)
  object ParentCall {
    def apply(qual: Option[Tree], name: String, tparams: List[TypeTree], args: List[List[Tree]]): Tree = ???
  }

  object Param {
    def apply(mods: List[Tree], name: String, tpe: Option[TypeTree], default: Option[Tree]): Tree = ???
  }

  object TypeParam {
    def apply(mods: List[Tree], name: String, tparams: List[TypeTree], tbounds: TypeTree, cbounds: List[TypeTree]): TypeTree = ???
  }

  // types
  object TypeIdent {
    def apply(name: String): TypeTree = ???
  }

  object TypeSelect {
    def apply(qual: Tree, name: String): TypeTree = ???
  }

  object TypeSingleton {
    def apply(ref: Tree): TypeTree = ???
  }

  object TypeApply {
    def apply(tpe: TypeTree, args: List[TypeTree]): TypeTree = ???
  }

  object TypeApplyInfix {
    def apply(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree = ???
  }

  object TypeFunction {
    def apply(params: List[TypeTree], res: TypeTree): TypeTree = ???
  }

  object TypeTuple {
    def apply(args: List[TypeTree]): TypeTree = ???
  }

  object TypeAnd {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree = ???
  }

  object TypeOr {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree = ???
  }

  object TypeRefine {
    def apply(tpe : TypeTree, stats: List[Tree]): TypeTree = ???
  }

  object TypeWildcard {
    def apply(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree = ???
  }

  object TypeBounds {
    def apply(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree = ???
  }

  object TypeRepeated {
    def apply(tpe: TypeTree): TypeTree = ???
  }

  object TypeByName {
    def apply(tpe: TypeTree): TypeTree = ???
  }

  object TypeAnnotated {
    def apply(tpe: TypeTree, annots: List[Tree]): TypeTree = ???
  }

  // terms
  object Lit {
    def apply(value: Any): Tree = ???
  }

  object Wildcard {
    def apply(): Tree = ???
  }

  object Ident {
    def apply(name: String): Tree = ???
  }

  object Select {
    def apply(qual: Tree, name: String): Tree = ???
  }

  object This {
    def apply(qual: String): Tree = ???
  }

  object Super {
    def apply(thisp: String, superp: String): Tree = ???
  }

  object Interpolate {
    def apply(prefix: String, parts: List[String], args: List[Tree]): Tree = ???
  }

  object Apply {
    def apply(fun: Tree, args: List[Tree]): Tree = ???
  }

  object ApplyType {
    def apply(fun: Tree, args: List[TypeTree]): Tree = ???
  }

  object Infix {
    def apply(lhs: Tree, op: String, rhs: Tree): Tree = ???
  }

  object Prefix {
    def apply(op: String, od: Tree): Tree = ???
  }

  object Postfix {
    def apply(od: Tree, op: String): Tree = ???
  }

  object Assign {
    def apply(lhs: Tree, rhs: Tree): Tree = ???
  }

  object Return {
    def apply(expr: Tree): Tree = ???
  }

  object Throw {
    def apply(expr: Tree): Tree = ???
  }

  object Ascribe {
    def apply(expr: Tree, tpe: Tree): Tree = ???
  }

  object Annotated {
    def apply(expr: Tree, annots: List[Tree]): Tree = ???
  }

  object Tuple {
    def apply(args: List[Tree]): Tree = ???
  }

  object Block {
    def apply(stats: List[Tree]): Tree = ???
  }

  object If {
    def apply(cond: Tree, thenp: Tree, elsep: Option[Tree]): Tree = ???
  }

  object Match {
    def apply(expr: Tree, cases: List[Tree]): Tree = ???
  }

  object Case {
    def apply(pat: Tree, cond: Option[Tree], body: Tree): Tree = ???
  }

  object Try {
    def apply(expr: Tree, cases: List[Tree], finallyp: Option[Tree]): Tree = ???
  }

  object TryWithTerm {
    def apply(expr: Tree, catchp: Tree, finallyp: Option[Tree]): Tree = ???
  }

  object Function {
    def apply(params: List[Tree], body: Tree): Tree = ???
  }

  object PartialFunction {
    def apply(cases: List[Tree]): Tree = ???
  }

  object While {
    def apply(expr: Tree, body: Tree): Tree = ???
  }

  object DoWhile {
    def apply(body: Tree, expr: Tree): Tree = ???
  }

  object For {
    def apply(enums: List[Tree], body: Tree): Tree = ???
  }

  object GenFrom {
    def apply(pat: Tree, rhs: Tree): Tree = ???
  }

  object GenAlias {
    def apply(pat: Tree, rhs: Tree): Tree = ???
  }

  object Guard {
    def apply(cond: Tree): Tree = ???
  }

  object Yield {
    def apply(expr: Tree): Tree = ???
  }

  object New {
    def apply(tpe: Tree): Tree = ???
  }

  object Named {
    def apply(name: String, expr: Tree): Tree = ???
  }

  object Repeated {
    def apply(expr: Tree): Tree = ???
  }

  // patterns
  object Bind {
    def apply(name: String, expr: Tree): Tree = ???
  }

  object Alternative {
    def apply(lhs: Tree, rhs: Tree): Tree = ???
  }

  // importees
  object Import {
    def apply(items: List[Tree]): Tree = ???
  }

  object ImportItem {
    def apply(ref: Tree, importees: List[Tree]): Tree = ???
  }

  object ImportName {
    def apply(name: String): Tree = ???
  }

  object ImportRename {
    def apply(from: String, to: String): Tree = ???
  }

  object ImportHide {
    def apply(name: String): Tree = ???
  }

  // modifiers
  object Mod {
    object Private {
      def apply(within: Tree): Tree = ???
    }

    object Protected {
      def apply(within: Tree): Tree = ???
    }

    object Val {
      def apply(): Tree = ???
    }

    object Var {
      def apply(): Tree = ???
    }

    object Implicit {
      def apply(): Tree = ???
    }

    object Final {
      def apply(): Tree = ???
    }

    object Sealed {
      def apply(): Tree = ???
    }

    object Override {
      def apply(): Tree = ???
    }

    object Abstract {
      def apply(): Tree = ???
    }

    object Lazy {
      def apply(): Tree = ???
    }

    object Inline {
      def apply(): Tree = ???
    }

    object Type {
      def apply(): Tree = ???
    }

    object Case {
      def apply(): Tree = ???
    }

    object Contravariant {
      def apply(): Tree = ???
    }

    object Covariant {
      def apply(): Tree = ???
    }

    object Annot {
      def apply(body: Tree): Tree = ???
    }
  }
}

