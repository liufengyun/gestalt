package scala.gestalt

import scala.collection.immutable.Seq

trait Modifiers[T] {
  def isPrivate: Boolean
  def setPrivate(within: String): Unit
  def getPrivate: String

  def isProtected: Boolean
  def setProtected(within: String): Unit
  def getProtected: String

  def isVal: Boolean
  def setVal: Unit

  def isVar: Boolean
  def setVar: Unit

  def isImplicit: Boolean
  def setImplicit: Unit

  def isFinal: Boolean
  def setFinal: Unit

  def isSealed: Boolean
  def setSealed: Unit

  def isOverride: Boolean
  def setOverride: Unit

  def isAbstract: Boolean
  def setAbstract: Unit

  def isLazy: Boolean
  def setLazy: Unit

  def isInline: Boolean
  def setInline: Unit

  def isCase: Boolean
  def setCase: Unit

  def isContravariant: Boolean
  def setContravariant: Unit

  def isCovariant: Boolean
  def setCovariant: Unit

  def addAnnot(annot: T): Unit
}

trait Toolbox {
  type Tree
  type TypeTree <: Tree      // safety by construction -- implementation can have TypeTree = Tree
  type Mods <: Modifiers[Tree]

  // diagnostics
  // TODO: should take pos as param -- need to introduce Pos as type param
  def error(message: String): Nothing = throw new Exception(message)

  // modifiers
  def emptyMods: Mods

  // definition trees
  def Object(mods: Mods, name: String, parents: Seq[Tree], selfOpt: Option[Tree], stats: Seq[Tree]): Tree
  def Class(mods: Mods, name: String, tparams: Seq[Tree], ctor: Option[Tree], parents: Seq[Tree], self: Option[Tree], stats: Seq[Tree]): Tree
  def AnonymClass(parents: Seq[Tree], self: Option[Tree], stats: Seq[Tree]): Tree
  def Trait(mods: Mods, name: String, tparams: Seq[Tree], ctor: Option[Tree], parents: Seq[Tree], self: Option[Tree], stats: Seq[Tree]): Tree
  def TypeDecl(mods: Mods, name: String, tparams: Seq[Tree], tbounds: Option[TypeTree]): Tree
  def TypeAlias(mods: Mods, name: String, tparams: Seq[Tree], rhs: TypeTree): Tree
  def DefDef(mods: Mods, name: String, tparams: Seq[Tree], paramss: Seq[Seq[Tree]], tpe: Option[TypeTree], rhs: Tree): Tree
  def DefDecl(mods: Mods, name: String, tparams: Seq[Tree], paramss: Seq[Seq[Tree]], tpe: TypeTree): Tree
  def ValDef(mods: Mods, name: String, tpe: Option[TypeTree], rhs: Tree): Tree
  def ValDef(mods: Mods, lhs: Tree, tpe: Option[TypeTree], rhs: Tree): Tree
  def ValDef(mods: Mods, pats: Seq[Tree], tpe: Option[TypeTree], rhs: Tree): Tree
  def ValDecl(mods: Mods, name: String, tpe: TypeTree): Tree
  def ValDecl(mods: Mods, vals: Seq[String], tpe: TypeTree): Tree
  def VarDef(mods: Mods, name: String, tpe: Option[TypeTree], rhs: Tree): Tree
  def VarDef(mods: Mods, lhs: Tree, tpe: Option[TypeTree], rhs: Tree): Tree
  def VarDef(mods: Mods, pats: Seq[Tree], tpe: Option[TypeTree], rhs: Tree): Tree
  def VarDecl(mods: Mods, name: String, tpe: TypeTree): Tree
  def VarDecl(mods: Mods, vars: Seq[String], tpe: TypeTree): Tree
  def Param(mods: Mods, name: String, tpe: Option[TypeTree], default: Option[Tree]): Tree
  def TypeParam(mods: Mods, name: String, tparams: Seq[TypeTree], tbounds: Option[TypeTree], cbounds: Seq[TypeTree]): TypeTree
  // extends qual.T[A, B](x, y)(z)
  def InitCall(qual: Option[Tree], name: String, tparams: Seq[TypeTree], argss: Seq[Seq[Tree]]): Tree
  def PrimaryCtor(mods: Mods, paramss: Seq[Seq[Tree]]): Tree
  def SecondaryCtor(mods: Mods, paramss: Seq[Seq[Tree]], rhs: Tree): Tree
  def Self(name: String, tpe: TypeTree): Tree
  def Self(name: String): Tree

  // type trees
  def TypeIdent(name: String): TypeTree
  def TypeSelect(qual: Tree, name: String): TypeTree
  def TypeSingleton(ref: Tree): TypeTree
  def TypeApply(tpe: TypeTree, args: Seq[TypeTree]): TypeTree
  def TypeApplyInfix(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree
  def TypeFunction(params: Seq[TypeTree], res: TypeTree): TypeTree
  def TypeTuple(args: Seq[TypeTree]): TypeTree
  def TypeAnd(lhs: TypeTree, rhs: TypeTree): TypeTree
  def TypeOr(lhs: TypeTree, rhs: TypeTree): TypeTree
  def TypeRefine(tpe : Option[TypeTree], stats: Seq[Tree]): TypeTree
  def TypeBounds(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree
  def TypeRepeated(tpe: TypeTree): TypeTree
  def TypeByName(tpe: TypeTree): TypeTree
  def TypeAnnotated(tpe: TypeTree, annots: Seq[Tree]): TypeTree


  // terms
  def Lit(value: Any): Tree
  def Ident(name: String): Tree
  def Select(qual: Tree, name: String): Tree
  def This(qual: String): Tree
  def Super(thisp: String, superp: String): Tree
  def Interpolate(prefix: String, parts: Seq[String], args: Seq[Tree]): Tree
  def Apply(fun: Tree, args: Seq[Tree]): Tree
  def ApplyType(fun: Tree, args: Seq[TypeTree]): Tree
  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  def Infix(lhs: Tree, op: String, rhs: Tree): Tree
  def Prefix(op: String, od: Tree): Tree
  def Postfix(od: Tree, op: String): Tree
  def Assign(lhs: Tree, rhs: Tree): Tree
  def Return(expr: Tree): Tree
  def Throw(expr: Tree): Tree
  def Ascribe(expr: Tree, tpe: Tree): Tree
  def Annotated(expr: Tree, annots: Seq[Tree]): Tree
  def Tuple(args: Seq[Tree]): Tree
  def Block(stats: Seq[Tree]): Tree
  def If(cond: Tree, thenp: Tree, elsep: Option[Tree]): Tree
  def Match(expr: Tree, cases: Seq[Tree]): Tree
  def Case(pat: Tree, cond: Option[Tree], body: Tree): Tree
  def Try(expr: Tree, cases: Seq[Tree], finallyp: Option[Tree]): Tree
  def Try(expr: Tree, handler: Tree, finallyp: Option[Tree]): Tree
  def Function(params: Seq[Tree], body: Tree): Tree
  def PartialFunction(cases: Seq[Tree]): Tree
  def While(expr: Tree, body: Tree): Tree
  def DoWhile(body: Tree, expr: Tree): Tree
  def For(enums: Seq[Tree], body: Tree): Tree
  def GenFrom(pat: Tree, rhs: Tree): Tree
  def GenAlias(pat: Tree, rhs: Tree): Tree
  def Guard(cond: Tree): Tree
  def Yield(expr: Tree): Tree
  // can be InitCall or AnonymClass
  def New(tpe: Tree): Tree
  def Named(name: String, expr: Tree): Tree
  def Repeated(expr: Tree): Tree

  // patterns
  def Bind(name: String, expr: Tree): Tree
  def Alternative(lhs: Tree, rhs: Tree): Tree

  // helpers
  def ApplySeq(fun: Tree, argss: Seq[Seq[Tree]]): Tree = argss match {
    case args :: rest => rest.foldLeft(Apply(fun, args)) { (acc, args) => Apply(acc, args) }
    case _ => Apply(fun, Nil)
  }

  // importees
  def Import(items: Seq[Tree]): Tree
  def ImportItem(ref: Tree, importees: Seq[Tree]): Tree
  def ImportName(name: String): Tree
  def ImportRename(from: String, to: String): Tree
  def ImportHide(name: String): Tree
}

/** StructToolbox defines extractors available for inspecting definition trees
 *
 *  To provide solid experience of macros, we only provide extractors for definition trees, like object, class, trait.
 *
 *  TODO:
 *    Provide definition tree transformers so that attachments (docs, etc) on the
 *    current tree is not an issue.
 */
trait StructToolbox extends Toolbox {
  val Object: ObjectHelper
  trait ObjectHelper {
    def unapply(tree: Tree): Option[(Mods, String, Seq[Tree], Option[Tree], Seq[Tree])]
  }

  val Class: ClassHelper
  trait ClassHelper {
    def unapply(tree: Tree): Option[(Mods, String, Seq[Tree], Option[Tree], Seq[Tree], Option[Tree], Seq[Tree])]
    // def cpy(tree: Tree)(mods: Mods, name: String, tparams: Seq[Tree], ctor: Option[Tree], parents: Seq[Tree], self: Option[Tree], stats: Seq[Tree])
  }

  val Trait: TraitHelper
  trait TraitHelper {
    def unapply(tree: Tree): Option[(Mods, String, Seq[Tree], Option[Tree], Seq[Tree], Option[Tree], Seq[Tree])]
  }

  val PrimaryCtor: PrimaryCtorHelper
  trait PrimaryCtorHelper {
    def unapply(tree: Tree): Option[(Mods, Seq[Seq[Tree]])]
  }
}

/** TypeToolbox defines extractors for inspecting expression trees as well as check types of trees
 */
trait TypeToolbox extends Toolbox { t =>
  type Tree <: { def tpe: Type }
  type Type

  // type operations
  implicit class TypeOps(val tp1: Type) {
    def =:=(tp2: Type) = t.=:=(tp1, tp2)
    def <:<(tp2: Type) = t.<:<(tp1, tp2)
  }

  def =:=(tp1: Type, tp2: Type): Boolean
  def <:<(tp1: Type, tp2: Type): Boolean
  def typeOf(path: String): Type

  val Lit: LitHelper
  trait LitHelper {
    def unapply(tree: Tree): Option[Any]
  }

  val Apply: ApplyHelper
  trait ApplyHelper {
    def unapply(tree: Tree): Option[(Tree, Seq[Tree])]
  }

  // helper
  object ApplySeq {
    def unapply(call: Tree):  Option[(Tree, Seq[Seq[Tree]])] = {
      def recur(acc: Seq[Seq[Tree]], term: Tree): (Tree, Seq[Seq[Tree]])  = term match {
        case Apply(fun, args) => recur(args +: acc, fun) // inner-most is in the front
        case fun => (fun, acc)
      }

      Some(recur(Nil, call))
    }
  }

}