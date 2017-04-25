package scala.gestalt

case class Location(fileName: String, line: Int, column: Int)

trait Toolbox extends Trees with Symbols with Types {
  // diagnostics - the implementation takes the position from the tree
  def error(message: String, tree: Tree): Unit

  // generate fresh name
  def fresh(prefix: String = "$local"): String
}

trait Trees {
  // safety by construction -- implementation can have TypeTree = Tree
  type Tree     >: Null <: AnyRef
  type TypeTree >: Null <: Tree
  type TermTree >: Null <: Tree
  type DefTree  >: Null <: Tree

  type Class     <: DefTree
  type Trait     <: DefTree
  type Object    <: DefTree
  type Param     <: DefTree
  type TypeParam <: DefTree
  type ValDef    <: DefTree
  type ValDecl   <: DefTree
  type DefDef    <: DefTree
  type DefDecl   <: DefTree
  type Self      <: DefTree
  type InitCall  <: Tree

  type Mods >: Null <: Modifiers

  trait Modifiers {
    def isPrivate: Boolean
    def isProtected: Boolean
    def isOverride: Boolean
    def isFinal: Boolean
    def isImplicit: Boolean
    def isLazy: Boolean
    def isSealed: Boolean
    def isAbstract: Boolean
    def isValParam: Boolean
    def isVarParam: Boolean
    def isCase: Boolean
    def isContravariant: Boolean
    def isCovariant: Boolean
    def isInline: Boolean
    def isMutable: Boolean
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
    def setValParam: Mods
    def setVarParam: Mods
    def setCase: Mods
    def setContravariant: Mods
    def setCovariant: Mods
    def setInline: Mods
    def setMutable: Mods

    def withAddedAnnotation(annot: Tree): Mods
    def withAnnotations(annots: Seq[Tree]): Mods
    def annotations: Seq[Tree]
  }

  // modifiers
  def emptyMods: Mods

  // definition trees
  def Object(mods: Mods, name: String, parents: Seq[InitCall], selfOpt: Option[Self], stats: Seq[Tree]): Object
  def Class(mods: Mods, name: String, tparams: Seq[TypeParam], ctorMods: Mods, paramss: Seq[Seq[Param]], parents: Seq[InitCall], self: Option[Self], stats: Seq[Tree]): Class
  def AnonymClass(parents: Seq[InitCall], self: Option[Self], stats: Seq[Tree]): DefTree
  def Trait(mods: Mods, name: String, tparams: Seq[TypeParam], ctorMods: Mods, paramss: Seq[Seq[Param]], parents: Seq[InitCall], self: Option[Self], stats: Seq[Tree]): Trait
  def TypeDecl(mods: Mods, name: String, tparams: Seq[TypeParam], tbounds: Option[TypeTree]): DefTree
  def TypeAlias(mods: Mods, name: String, tparams: Seq[TypeParam], rhs: TypeTree): DefTree
  def DefDef(mods: Mods, name: String, tparams: Seq[TypeParam], paramss: Seq[Seq[Param]], tpe: Option[TypeTree], rhs: Tree): DefDef
  def DefDecl(mods: Mods, name: String, tparams: Seq[TypeParam], paramss: Seq[Seq[Param]], tpe: TypeTree): DefDecl
  def ValDef(mods: Mods, name: String, tpe: Option[TypeTree], rhs: Tree): ValDef
  def PatDef(mods: Mods, lhs: TermTree, tpe: Option[TypeTree], rhs: Tree): DefTree
  def SeqDef(mods: Mods, pats: Seq[TermTree], tpe: Option[TypeTree], rhs: Tree): DefTree
  def ValDecl(mods: Mods, name: String, tpe: TypeTree): ValDecl
  def SeqDecl(mods: Mods, vals: Seq[String], tpe: TypeTree): DefTree
  def Param(mods: Mods, name: String, tpe: Option[TypeTree], default: Option[Tree]): Param
  def TypeParam(mods: Mods, name: String, tparams: Seq[TypeParam], tbounds: Option[TypeTree], cbounds: Seq[TypeTree]): TypeParam
  // extends qual.T[A, B](x, y)(z)
  def InitCall(qual: Option[Tree], name: String, targs: Seq[TypeTree], argss: Seq[Seq[TermTree]]): InitCall
  def SecondaryCtor(mods: Mods, paramss: Seq[Seq[Param]], rhs: TermTree): Tree
  def Self(name: String, tpe: TypeTree): Self
  def Self(name: String): Self

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
  def Lit(value: Any): TermTree
  def Ident(name: String): TermTree
  def Select(qual: TermTree, name: String): TermTree
  def This(qual: String): TermTree
  def Super(thisp: String, superp: String): TermTree
  def Interpolate(prefix: String, parts: Seq[String], args: Seq[TermTree]): TermTree
  def Apply(fun: TermTree, args: Seq[TermTree]): TermTree
  def ApplyType(fun: TermTree, args: Seq[TypeTree]): TermTree
  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  def Infix(lhs: TermTree, op: String, rhs: TermTree): TermTree
  def Prefix(op: String, od: TermTree): TermTree
  def Postfix(od: TermTree, op: String): TermTree
  def Assign(lhs: TermTree, rhs: TermTree): TermTree
  def Return(expr: TermTree): TermTree
  def Return: TermTree
  def Throw(expr: TermTree): TermTree
  def Ascribe(expr: TermTree, tpe: TypeTree): TermTree
  def Annotated(expr: TermTree, annots: Seq[Tree]): TermTree
  def Tuple(args: Seq[TermTree]): TermTree
  def Block(stats: Seq[Tree]): TermTree
  def If(cond: TermTree, thenp: TermTree, elsep: Option[TermTree]): TermTree
  def Match(expr: TermTree, cases: Seq[Tree]): TermTree
  def Case(pat: TermTree, cond: Option[TermTree], body: TermTree): Tree
  def Try(expr: Tree, cases: Seq[Tree], finallyp: Option[TermTree]): TermTree
  def Try(expr: TermTree, handler: Tree, finallyp: Option[TermTree]): Tree
  def Function(params: Seq[Param], body: TermTree): TermTree
  def PartialFunction(cases: Seq[Tree]): TermTree
  def While(expr: TermTree, body: TermTree): TermTree
  def DoWhile(body: TermTree, expr: TermTree): TermTree
  def For(enums: Seq[Tree], body: TermTree): TermTree
  def ForYield(enums: Seq[Tree], body: TermTree): TermTree
  def GenFrom(pat: TermTree, rhs: TermTree): Tree
  def GenAlias(pat: TermTree, rhs: TermTree): Tree
  def Guard(cond: TermTree): Tree
  // can be InitCall or AnonymClass
  def New(tpe: Tree): TermTree
  def Named(name: String, expr: Tree): TermTree
  def Repeated(expr: Tree): TermTree

  // patterns
  def Bind(name: String, expr: TermTree): TermTree
  def Alternative(trees: Seq[TermTree]): TermTree

  // importees
  def Import(items: Seq[Tree]): Tree
  def ImportItem(prefix: Tree, importees: Seq[Tree]): Tree
  def ImportName(name: String): Tree
  def ImportRename(from: String, to: String): Tree
  def ImportHide(name: String): Tree

  // extractors
  val Lit: LitHelper
  trait LitHelper {
    def unapply(tree: Tree): Option[Any]
  }

  val Apply: ApplyHelper
  trait ApplyHelper {
    def unapply(tree: Tree): Option[(TermTree, Seq[TermTree])]
  }

  val ApplyType: ApplyTypeHelper
  trait ApplyTypeHelper {
    def unapply(tree: Tree): Option[(TermTree, Seq[TypeTree])]
  }

  val Ident: IdentHelper
  trait IdentHelper {
    def unapply(tree: Tree): Option[String]
  }

  val This: ThisHelper
  trait ThisHelper {
    def unapply(tree: Tree): Option[String]
  }

  val Select: SelectHelper
  trait SelectHelper {
    def unapply(tree: Tree): Option[(TermTree, String)]
  }

  val Ascribe: AscribeHelper
  trait AscribeHelper {
    def unapply(tree: Tree): Option[(TermTree, TypeTree)]
  }

  val Assign: AssignHelper
  trait AssignHelper {
    def unapply(tree: Tree): Option[(TermTree, TermTree)]
  }

  val Annotated: AnnotatedHelper
  trait AnnotatedHelper {
    def unapply(tree: Tree): Option[(TermTree, Seq[Tree])]
  }

  val Block: BlockHelper
  trait BlockHelper {
    def unapply(tree: Tree): Option[Seq[Tree]]
  }

  val PartialFunction: PartialFunctionHelper
  trait PartialFunctionHelper {
    def unapply(tree: Tree): Option[Seq[Tree]]
  }

  val Tuple: TupleHelper
  trait TupleHelper {
    def unapply(tree: Tree): Option[Seq[TermTree]]
  }

  val SeqLiteral: SeqLiteralHelper
  trait SeqLiteralHelper {
    def unapply(tree: Tree): Option[Seq[TermTree]]
  }

  val Object: ObjectHelper
  trait ObjectHelper {
    def unapply(tree: Tree): Option[(Mods, String, Seq[InitCall], Option[Self], Seq[Tree])]
  }

  val Class: ClassHelper
  trait ClassHelper {
    def unapply(tree: Tree): Option[(Mods, String, Seq[TypeParam], Mods, Seq[Seq[Param]], Seq[InitCall], Option[Self], Seq[Tree])]
  }

  val Trait: TraitHelper
  trait TraitHelper {
    def unapply(tree: Tree): Option[(Mods, String, Seq[TypeParam], Mods, Seq[Seq[Param]], Seq[InitCall], Option[Self], Seq[Tree])]
  }

  // accessors for definition trees
  implicit def toParamRep(tree: Param): ParamRep
  trait ParamRep {
    def mods: Mods
    def name: String
    def tpt: Option[TypeTree]
    def default: Option[Tree]
    def copy(name: String = this.name, mods: Mods = this.mods,
             tptOpt: Option[TypeTree] = this.tpt, defaultOpt: Option[Tree] = this.default): Param
  }

  implicit def toTypeParamRep(tree: TypeParam): TypeParamRep
  trait TypeParamRep {
    def mods: Mods
    def name: String
    def tparams: Seq[TypeParam]
  }

  implicit def toClassRep(tree: Class): ClassRep
  trait ClassRep {
    def mods: Mods
    def ctorMods: Mods
    def name: String
    def tparams: Seq[TypeParam]
    def paramss: Seq[Seq[Param]]
    def self: Option[Self]
    def stats: Seq[Tree]

    def copy(mods: Mods = this.mods, paramss: Seq[Seq[Param]] = this.paramss, stats: Seq[Tree] = this.stats): Class
  }

  val ClassRep: ClassRepHelper
  trait ClassRepHelper {
    def unapply(tree: Tree): Option[ClassRep]
  }

  implicit def toValDefRep(tree: ValDef): ValDefRep
  trait ValDefRep {
    def mods: Mods
    def name: String
    def tpt: Option[TypeTree]
    def rhs: Tree
    def copy(name: String = this.name, mods: Mods = this.mods,
             tptOpt: Option[TypeTree] = this.tpt, rhs: Tree = this.rhs): ValDef
  }

  val ValDefRep: ValDefRepHelper
  trait ValDefRepHelper {
    def unapply(tree: Tree): Option[ValDefRep]
  }

  implicit def toValDeclRep(tree: ValDecl): ValDeclRep
  trait ValDeclRep {
    def mods: Mods
    def name: String
    def tpt: TypeTree
    def copy(name: String = this.name, mods: Mods = this.mods, tpt: TypeTree = this.tpt): ValDecl
  }

  val ValDeclRep: ValDeclRepHelper
  trait ValDeclRepHelper {
    def unapply(tree: Tree): Option[ValDeclRep]
  }

  implicit def toDefDefRep(tree: DefDef): DefDefRep
  trait DefDefRep {
    def mods: Mods
    def tparams: Seq[TypeParam]
    def paramss: Seq[Seq[Param]]
    def name: String
    def tpt: Option[TypeTree]
    def rhs: Tree
    def copy(name: String = this.name, mods: Mods = this.mods,
             tptOpt: Option[TypeTree] = this.tpt, rhs: Tree = this.rhs): DefDef
  }

  val DefDefRep: DefDefRepHelper
  trait DefDefRepHelper {
    def unapply(tree: Tree): Option[DefDefRep]
  }

  implicit def toDefDeclRep(tree: DefDecl): DefDeclRep
  trait DefDeclRep {
    def mods: Mods
    def tparams: Seq[TypeParam]
    def paramss: Seq[Seq[Param]]
    def name: String
    def tpt: TypeTree
    def copy(name: String = this.name, mods: Mods = this.mods, tpt: TypeTree = this.tpt): DefDecl
  }

  val DefDeclRep: DefDeclRepHelper
  trait DefDeclRepHelper {
    def unapply(tree: Tree): Option[DefDeclRep]
  }

  // helpers
  def ApplySeq(fun: TermTree, argss: Seq[Seq[TermTree]]): Tree = argss match {
    case args :: rest => rest.foldLeft(Apply(fun, args)) { (acc, args) => Apply(acc, args) }
    case _ => Apply(fun, Nil)
  }

  object ApplySeq {
    def unapply(call: TermTree):  Option[(Tree, Seq[Seq[TermTree]])] = {
      def recur(acc: Seq[Seq[TermTree]], term: TermTree): (TermTree, Seq[Seq[TermTree]])  = term match {
        case Apply(fun, args) => recur(args +: acc, fun) // inner-most is in the front
        case fun => (fun, acc)
      }

      Some(recur(Nil, call))
    }
  }

}

trait Types { this: Toolbox =>
  type Type

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
  def caseFields(tp: Type): Seq[Symbol]

  /* field with the given name */
  def field(tp: Type, name: String): Option[Symbol]
}


trait Symbols { this: Toolbox =>
  type Symbol

  /** name of a member */
  def name(mem: Symbol): String

  /** type of a member with respect to a prefix */
  def asSeenFrom(mem: Symbol, prefix: Type): Type
}
