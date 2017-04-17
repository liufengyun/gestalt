package scala.gestalt

case class Location(fileName: String, line: Int, column: Int)

trait Toolbox {
  type Tree >: Null
  type TypeTree <: Tree      // safety by construction -- implementation can have TypeTree = Tree
  type Mods <: Modifiers

  trait Modifiers {
    def isPrivate: Boolean
    def isProtected: Boolean
    def isOverride: Boolean
    def isFinal: Boolean
    def isImplicit: Boolean
    def isLazy: Boolean
    def isSealed: Boolean
    def isAbstract: Boolean
    def isMutable: Boolean
    def isCase: Boolean
    def isContravariant: Boolean
    def isCovariant: Boolean
    def isInline: Boolean
    def privateWithin: String
    def hasAnnotations: Boolean

    // can be empty or `this`
    def setPrivate(within: String): Mods
    def setProtected(within: String): Mods
    def setOverride: Mods
    def setFinal: Mods
    def setImplicit: Mods
    def setLazy: Mods
    def setSealed: Mods
    def setAbstract: Mods
    def setMutable: Mods
    def setCase: Mods
    def setContravariant: Mods
    def setCovariant: Mods
    def setInline: Mods

    def withAddedAnnotation(annot: Tree): Mods
  }

  // diagnostics - the implementation takes the position from the tree
  def error(message: String, tree: Tree): Unit

  // generate fresh name
  def fresh(prefix: String = "$local"): String

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

/** TypeToolbox defines extractors for inspecting expression trees as well as APIs for types
 */
trait TypeToolbox extends Toolbox { t =>
  type Type
  type Member

  /** get the location where the def macro is used */
  def currentLocation: Location

  /** are the two types equal? */
  def =:=(tp1: Type, tp2: Type): Boolean

  /** is `tp1` a subtype of `tp2` */
  def <:<(tp1: Type, tp2: Type): Boolean

  /** returning a type referring to a type definition */
  def typeRef(path: String): Type

  /** returning a type referring to a value definition */
  def termRef(path: String): Type

  /** type associated with the tree */
  def typeOf(tree: Tree): Type

  /** does the type refer to a case class? */
  def isCaseClass(tp: Type): Boolean

  /** val fields of a case class Type -- only the ones declared in primary constructor */
  def caseFields(tp: Type): Seq[Member]

  /* field with the given name */
  def field(tp: Type, name: String): Option[Member]

  /** name of a member */
  def name(mem: Member): String

  /** type of a member with respect to a prefix */
  def asSeenFrom(mem: Member, prefix: Type): Type

  val Ascribe: AscribeHelper
  trait AscribeHelper {
    def unapply(arg: Tree): Option[(Tree, TypeTree)]
  }

  val SeqLiteral: SeqLiteralHelper
  trait SeqLiteralHelper {
    def unapply(tree: Tree): Option[Seq[Tree]]
  }

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
