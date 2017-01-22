package scala.gestalt

trait Toolbox { t =>
  // portable trees -- minimum assumptions
  type Tree <: { def tpe: Type } // TODO: structural types performance penalty.
  type TypeTree <: Tree          // avoid errors in mixing type and term -- implementation can have TypeTree = Tree
  type Type

  // type operations
  implicit class TypeHelper(val tp1: Type) {
    def =:=(tp2: Type) = t.=:=(tp1, tp2)
    def <:<(tp2: Type) = t.<:<(tp1, tp2)
  }

  def =:=(tp1: Type, tp2: Type): Boolean
  def <:<(tp1: Type, tp2: Type): Boolean
  def typeOf(path: String): Type

  // diagnostics
  // TODO: should take pos as param -- need to introduce Pos as type param
  def error(message: String): Nothing = throw new Exception(message)

  // standard constructors and extractors
  object Object {
    def apply(mods: Seq[Tree], name: String, parents: Seq[Tree], self: Option[Tree], stats: Option[Seq[Tree]]): Tree = ???
    def unapply(tree: Tree): Option[(Seq[Tree], String, Seq[Tree], Option[Tree], Option[Seq[Tree]])] = ???
  }

  object Class {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], ctor: Option[Tree], parents: Seq[Tree], self: Option[Tree], stats: Option[Seq[Tree]]): Tree = ???
    def unapply(tree: Tree): Option[(Seq[Tree], String, Seq[Tree], Option[Tree], Seq[Tree], Option[Tree], Option[Seq[Tree]])] = ???
  }

  object AnonymClass {
    def apply(parents: Seq[Tree], self: Option[Tree], stats: Option[Seq[Tree]]): Tree = ???
  }

  object Trait {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], ctor: Option[Tree], parents: Seq[Tree], self: Option[Tree], stats: Option[Seq[Tree]]): Tree = ???
    def unapply(tree: Tree): Option[(Seq[Tree], String, Seq[Tree], Option[Tree], Seq[Tree], Option[Tree], Option[Seq[Tree]])] = ???
  }

  object Type {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], rhs: TypeTree): Tree = ???
  }

  object DefDef {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], paramss: Seq[Seq[Tree]], tpe: Option[TypeTree], rhs: Option[Tree]): Tree = ???
  }

  object ValDef {
    def apply(mods: Seq[Tree], name: String, tpe: Option[TypeTree], rhs: Option[Tree]): Tree = ???
    def apply(mods: Seq[Tree], lhs: Tree, tpe: Option[TypeTree], rhs: Option[Tree]): Tree = ???
    def apply(mods: Seq[Tree], lhs: Seq[Tree], tpe: Option[TypeTree], rhs: Option[Tree]): Tree = ???
  }

  object PrimaryCtor {
    def apply(mods: Seq[Tree], paramss: Seq[Seq[Tree]]): Tree = ???
  }

  object SecondaryCtor {
    def apply(mods: Seq[Tree], paramss: Seq[Seq[Tree]], rhs: Tree): Tree = ???
  }

  // qual.T[A, B](x, y)(z)
  object InitCall {
    def apply(qual: Option[Tree], name: String, tparams: Seq[TypeTree], argss: Seq[Seq[Tree]]): Tree = ???
  }

  object Param {
    def apply(mods: Seq[Tree], name: String, tpe: Option[TypeTree], default: Option[Tree]): Tree = ???
  }

  object TypeParam {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[TypeTree], tbounds: TypeTree, cbounds: Seq[TypeTree]): TypeTree = ???
  }

  object Self {
    def apply(name: String, tpe: Option[TypeTree]): Tree = ???
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
    def apply(tpe: TypeTree, args: Seq[TypeTree]): TypeTree = ???
  }

  object TypeApplyInfix {
    def apply(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree = ???
  }

  object TypeFunction {
    def apply(params: Seq[TypeTree], res: TypeTree): TypeTree = ???
  }

  object TypeTuple {
    def apply(args: Seq[TypeTree]): TypeTree = ???
  }

  object TypeAnd {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree = ???
  }

  object TypeOr {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree = ???
  }

  object TypeRefine {
    def apply(tpe : Option[TypeTree], stats: Seq[Tree]): TypeTree = ???
  }

  object TypeWildcard {
    def apply(): TypeTree = apply(TypeBounds(None, None))
    def apply(bounds: t.Tree): TypeTree = ???
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
    def apply(tpe: TypeTree, annots: Seq[Tree]): TypeTree = ???
  }

  // terms
  object Lit {
    def apply(value: Any): Tree = ???
    def unapply(value: Any): Option[Any] = ???
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
    def apply(prefix: String, parts: Seq[String], args: Seq[Tree]): Tree = ???
  }

  object Apply {
    def apply(fun: Tree, args: Seq[Tree]): Tree = ???
    def unapply(tree: Tree): Option[(Tree, Seq[Tree])] = ???
  }

  // helper
  object ApplySeq {
    def apply(fun: Tree, argss: Seq[Seq[Tree]]): Tree = argss match {
      case args :: rest => rest.foldLeft(Apply(fun, args)) { (acc, args) => Apply(acc, args) }
      case _ => Apply(fun, Nil)
    }

    def unapply(call: Tree):  Option[(Tree, Seq[Seq[Tree]])] = {
      def recur(acc: Seq[Seq[Tree]], term: Tree): (Tree, Seq[Seq[Tree]])  = term match {
        case Apply(fun, args) => recur(args +: acc, fun) // inner-most is in the front
        case fun => (fun, acc)
      }

      Some(recur(Nil, call))
    }
  }

  object ApplyType {
    def apply(fun: Tree, args: Seq[TypeTree]): Tree = ???
  }

  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
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
    def apply(expr: Tree, annots: Seq[Tree]): Tree = ???
  }

  object Tuple {
    def apply(args: Seq[Tree]): Tree = ???
  }

  object Block {
    def apply(stats: Seq[Tree]): Tree = ???
  }

  object If {
    def apply(cond: Tree, thenp: Tree, elsep: Option[Tree]): Tree = ???
  }

  object Match {
    def apply(expr: Tree, cases: Seq[Tree]): Tree = ???
  }

  object Case {
    def apply(pat: Tree, cond: Option[Tree], body: Tree): Tree = ???
  }

  object Try {
    def apply(expr: Tree, cases: Seq[Tree], finallyp: Option[Tree]): Tree = ???
    def apply(expr: Tree, catchp: Tree, finallyp: Option[Tree]): Tree = ???
  }

  object Function {
    def apply(params: Seq[Tree], body: Tree): Tree = ???
  }

  object PartialFunction {
    def apply(cases: Seq[Tree]): Tree = ???
  }

  object While {
    def apply(expr: Tree, body: Tree): Tree = ???
  }

  object DoWhile {
    def apply(body: Tree, expr: Tree): Tree = ???
  }

  object For {
    def apply(enums: Seq[Tree], body: Tree): Tree = ???
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

  // can be InitCall or AnonymClass
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
    def apply(items: Seq[Tree]): Tree = ???
  }

  object ImportItem {
    def apply(ref: Tree, importees: Seq[Tree]): Tree = ???
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

