package scala.gestalt

import scala.collection.immutable.Seq

trait Toolbox { t =>
  // portable trees -- minimum assumptions
  type Tree <: { def tpe: Type } // TODO: structural types performance penalty.
  type TypeTree <: Tree          // safety by construction -- implementation can have TypeTree = Tree
  type Type

  // type operations
  implicit class TypeOps(val tp1: Type) {
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
  val Object: ObjectHelper
  trait ObjectHelper {
    def apply(mods: Seq[Tree], name: String, parents: Seq[Tree], selfOpt: Option[Tree], stats: Seq[Tree]): Tree
    def unapply(tree: Tree): Option[(Seq[Tree], String, Seq[Tree], Option[Tree], Seq[Tree])]
  }

  val Class: ClassHelper
  trait ClassHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], ctor: Option[Tree], parents: Seq[Tree], self: Option[Tree], stats: Seq[Tree]): Tree
    def unapply(tree: Tree): Option[(Seq[Tree], String, Seq[Tree], Option[Tree], Seq[Tree], Option[Tree], Seq[Tree])]
  }

  val AnonymClass: AnonymClassHelper
  trait AnonymClassHelper {
    def apply(parents: Seq[Tree], self: Option[Tree], stats: Seq[Tree]): Tree
  }

  val Trait: TraitHelper
  trait TraitHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], ctor: Option[Tree], parents: Seq[Tree], self: Option[Tree], stats: Seq[Tree]): Tree
    def unapply(tree: Tree): Option[(Seq[Tree], String, Seq[Tree], Option[Tree], Seq[Tree], Option[Tree], Seq[Tree])]
  }

  val TypeDecl: TypeDeclHelper
  trait TypeDeclHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], tbounds: Option[TypeTree]): Tree
  }

  val TypeAlias: TypeAliasHelper
  trait TypeAliasHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], rhs: TypeTree): Tree
  }

  val DefDef: DefDefHelper
  trait DefDefHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], paramss: Seq[Seq[Tree]], tpe: Option[TypeTree], rhs: Tree): Tree
  }

  val DefDecl: DefDeclHelper
  trait DefDeclHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[Tree], paramss: Seq[Seq[Tree]], tpe: TypeTree): Tree
  }

  val ValDef: ValDefHelper
  trait ValDefHelper {
    def apply(mods: Seq[Tree], name: String, tpe: Option[TypeTree], rhs: Tree): Tree
    def apply(mods: Seq[Tree], lhs: Tree, tpe: Option[TypeTree], rhs: Tree): Tree
    def apply(mods: Seq[Tree], pats: Seq[Tree], tpe: Option[TypeTree], rhs: Tree): Tree
  }

  val ValDecl: ValDeclHelper
  trait ValDeclHelper {
    def apply(mods: Seq[Tree], name: String, tpe: TypeTree): Tree
    def apply(mods: Seq[Tree], vals: Seq[String], tpe: TypeTree): Tree
  }

  val VarDef: VarDefHelper
  trait VarDefHelper {
    def apply(mods: Seq[Tree], name: String, tpe: Option[TypeTree], rhs: Tree): Tree
    def apply(mods: Seq[Tree], lhs: Tree, tpe: Option[TypeTree], rhs: Tree): Tree
    def apply(mods: Seq[Tree], pats: Seq[Tree], tpe: Option[TypeTree], rhs: Tree): Tree
  }

  val VarDecl: VarDeclHelper
  trait VarDeclHelper {
    def apply(mods: Seq[Tree], name: String, tpe: TypeTree): Tree
    def apply(mods: Seq[Tree], vars: Seq[String], tpe: TypeTree): Tree
  }

  val PrimaryCtor: PrimaryCtorHelper
  trait PrimaryCtorHelper {
    def apply(mods: Seq[Tree], paramss: Seq[Seq[Tree]]): Tree
    def unapply(tree: Tree): Option[(Seq[Tree], Seq[Seq[Tree]])]
  }

  val SecondaryCtor: SecondaryCtorHelper
  trait SecondaryCtorHelper {
    def apply(mods: Seq[Tree], paramss: Seq[Seq[Tree]], rhs: Tree): Tree
  }

  // qual.T[A, B](x, y)(z)
  val InitCall: InitCallHelper
  trait InitCallHelper {
    def apply(qual: Option[Tree], name: String, tparams: Seq[TypeTree], argss: Seq[Seq[Tree]]): Tree
  }

  val Param: ParamHelper
  trait ParamHelper {
    def apply(mods: Seq[Tree], name: String, tpe: Option[TypeTree], default: Option[Tree]): Tree
    def unapply(tree: Tree): Option[(Seq[Tree], String, Option[TypeTree], Option[Tree])]

    def copy(param: Tree)(
      mods: Seq[Tree]       = unapply(param).get._1,
      name: String          = unapply(param).get._2,
      tpe: Option[TypeTree] = unapply(param).get._3,
      default: Option[Tree] = unapply(param).get._4
    ) = apply(mods, name, tpe, default)
  }

  val TypeParam: TypeParamHelper
  trait TypeParamHelper {
    def apply(mods: Seq[Tree], name: String, tparams: Seq[TypeTree], tbounds: Option[TypeTree], cbounds: Seq[TypeTree]): TypeTree
  }

  val Self: SelfHelper
  trait SelfHelper {
    def apply(name: String, tpe: TypeTree): Tree
    def apply(name: String): Tree
  }

  // types
  val TypeIdent: TypeIdentHelper
  trait TypeIdentHelper {
    def apply(name: String): TypeTree
  }

  val TypeSelect: TypeSelectHelper
  trait TypeSelectHelper {
    def apply(qual: Tree, name: String): TypeTree
  }

  val TypeSingleton: TypeSingletonHelper
  trait TypeSingletonHelper {
    def apply(ref: Tree): TypeTree
  }

  val TypeApply: TypeApplyHelper
  trait TypeApplyHelper {
    def apply(tpe: TypeTree, args: Seq[TypeTree]): TypeTree
  }

  val TypeApplyInfix: TypeApplyInfixHelper
  trait TypeApplyInfixHelper {
    def apply(lhs: TypeTree, op: String, rhs: TypeTree): TypeTree
  }

  val TypeFunction: TypeFunctionHelper
  trait TypeFunctionHelper {
    def apply(params: Seq[TypeTree], res: TypeTree): TypeTree
  }

  val TypeTuple: TypeTupleHelper
  trait TypeTupleHelper {
    def apply(args: Seq[TypeTree]): TypeTree
  }

  val TypeAnd: TypeAndHelper
  trait TypeAndHelper {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree
  }

  val TypeOr: TypeOrHelper
  trait TypeOrHelper {
    def apply(lhs: TypeTree, rhs: TypeTree): TypeTree
  }

  val TypeRefine: TypeRefineHelper
  trait TypeRefineHelper {
    def apply(tpe : Option[TypeTree], stats: Seq[Tree]): TypeTree
  }

  val TypeBounds: TypeBoundsHelper
  trait TypeBoundsHelper {
    def apply(lo: Option[TypeTree], hi: Option[TypeTree]): TypeTree
  }

  val TypeRepeated: TypeRepeatedHelper
  trait TypeRepeatedHelper {
    def apply(tpe: TypeTree): TypeTree
  }

  val TypeByName: TypeByNameHelper
  trait TypeByNameHelper {
    def apply(tpe: TypeTree): TypeTree
  }

  val TypeAnnotated: TypeAnnotatedHelper
  trait TypeAnnotatedHelper {
    def apply(tpe: TypeTree, annots: Seq[Tree]): TypeTree
  }

  // terms
  val Lit: LitHelper
  trait LitHelper {
    def apply(value: Any): Tree
    def unapply(tree: Tree): Option[Any]
  }

  val Ident: IdentHelper
  trait IdentHelper {
    def apply(name: String): Tree
  }

  val Select: SelectHelper
  trait SelectHelper {
    def apply(qual: Tree, name: String): Tree
  }

  val This: ThisHelper
  trait ThisHelper {
    def apply(qual: String): Tree
  }

  val Super: SuperHelper
  trait SuperHelper {
    def apply(thisp: String, superp: String): Tree
  }

  val Interpolate: InterpolateHelper
  trait InterpolateHelper {
    def apply(prefix: String, parts: Seq[String], args: Seq[Tree]): Tree
  }

  val Apply: ApplyHelper
  trait ApplyHelper {
    def apply(fun: Tree, args: Seq[Tree]): Tree
    def unapply(tree: Tree): Option[(Tree, Seq[Tree])]
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

  val ApplyType: ApplyTypeHelper
  trait ApplyTypeHelper {
    def apply(fun: Tree, args: Seq[TypeTree]): Tree
    def unapply(tree: Tree): Option[(Tree, Seq[TypeTree])]
  }

  // a + (b, c)  =>  Infix(a, +, Tuple(b, c))
  val Infix: InfixHelper
  trait InfixHelper {
    def apply(lhs: Tree, op: String, rhs: Tree): Tree
  }

  val Prefix: PrefixHelper
  trait PrefixHelper {
    def apply(op: String, od: Tree): Tree
  }

  val Postfix: PostfixHelper
  trait PostfixHelper {
    def apply(od: Tree, op: String): Tree
  }

  val Assign: AssignHelper
  trait AssignHelper {
    def apply(lhs: Tree, rhs: Tree): Tree
    def unapply(tree: Tree): Option[(Tree, Tree)]
  }

  val Return: ReturnHelper
  trait ReturnHelper {
    def apply(expr: Tree): Tree
  }

  val Throw: ThrowHelper
  trait ThrowHelper {
    def apply(expr: Tree): Tree
  }

  val Ascribe: AscribeHelper
  trait AscribeHelper {
    def apply(expr: Tree, tpe: Tree): Tree
  }

  val Annotated: AnnotatedHelper
  trait AnnotatedHelper {
    def apply(expr: Tree, annots: Seq[Tree]): Tree
  }

  val Tuple: TupleHelper
  trait TupleHelper {
    def apply(args: Seq[Tree]): Tree
  }

  val Block: BlockHelper
  trait BlockHelper {
    def apply(stats: Seq[Tree]): Tree
  }

  val If: IfHelper
  trait IfHelper {
    def apply(cond: Tree, thenp: Tree, elsep: Option[Tree]): Tree
  }

  val Match: MatchHelper
  trait MatchHelper {
    def apply(expr: Tree, cases: Seq[Tree]): Tree
  }

  val Case: CaseHelper
  trait CaseHelper {
    def apply(pat: Tree, cond: Option[Tree], body: Tree): Tree
  }

  val Try: TryHelper
  trait TryHelper {
    def apply(expr: Tree, cases: Seq[Tree], finallyp: Option[Tree]): Tree
    def apply(expr: Tree, handler: Tree, finallyp: Option[Tree]): Tree
  }

  val Function: FunctionHelper
  trait FunctionHelper {
    def apply(params: Seq[Tree], body: Tree): Tree
  }

  val PartialFunction: PartialFunctionHelper
  trait PartialFunctionHelper {
    def apply(cases: Seq[Tree]): Tree
  }

  val While: WhileHelper
  trait WhileHelper {
    def apply(expr: Tree, body: Tree): Tree
  }

  val DoWhile: DoWhileHelper
  trait DoWhileHelper {
    def apply(body: Tree, expr: Tree): Tree
  }

  val For: ForHelper
  trait ForHelper {
    def apply(enums: Seq[Tree], body: Tree): Tree
  }

  val GenFrom: GenFromHelper
  trait GenFromHelper {
    def apply(pat: Tree, rhs: Tree): Tree
  }

  val GenAlias: GenAliasHelper
  trait GenAliasHelper {
    def apply(pat: Tree, rhs: Tree): Tree
  }

  val Guard: GuardHelper
  trait GuardHelper {
    def apply(cond: Tree): Tree
  }

  val Yield: YieldHelper
  trait YieldHelper {
    def apply(expr: Tree): Tree
  }

  // can be InitCall or AnonymClass
  val New: NewHelper
  trait NewHelper {
    def apply(tpe: Tree): Tree
  }

  val Named: NamedHelper
  trait NamedHelper {
    def apply(name: String, expr: Tree): Tree
  }

  val Repeated: RepeatedHelper
  trait RepeatedHelper {
    def apply(expr: Tree): Tree
  }

  // patterns
  val Bind: BindHelper
  trait BindHelper {
    def apply(name: String, expr: Tree): Tree
  }

  val Alternative: AlternativeHelper
  trait AlternativeHelper {
    def apply(lhs: Tree, rhs: Tree): Tree
  }

  // importees
  val Import: ImportHelper
  trait ImportHelper {
    def apply(items: Seq[Tree]): Tree
  }

  val ImportItem: ImportItemHelper
  trait ImportItemHelper {
    def apply(ref: Tree, importees: Seq[Tree]): Tree
  }

  val ImportName: ImportNameHelper
  trait ImportNameHelper {
    def apply(name: String): Tree
  }

  val ImportRename: ImportRenameHelper
  trait ImportRenameHelper {
    def apply(from: String, to: String): Tree
  }

  val ImportHide: ImportHideHelper
  trait ImportHideHelper {
    def apply(name: String): Tree
  }

  // modifiers
  val Mod: ModHelper
  trait ModHelper {
    val Private: PrivateHelper
    trait PrivateHelper {
      def apply(within: String): Tree
      def unapply(tree: Tree): Option[String]
    }

    val Protected: ProtectedHelper
    trait ProtectedHelper {
      def apply(within: String): Tree
      def unapply(tree: Tree): Option[String]
    }

    val Val: ValHelper
    trait ValHelper {
      def apply(): Tree
      def unapply(tree: Tree): Boolean
    }

    val Var: VarHelper
    trait VarHelper {
      def apply(): Tree
      def unapply(tree: Tree): Boolean
    }

    val Implicit: ImplicitHelper
    trait ImplicitHelper {
      def apply(): Tree
      def unapply(tree: Tree): Boolean
    }

    val Final: FinalHelper
    trait FinalHelper {
      def apply(): Tree
      def unapply(tree: Tree): Boolean
    }

    val Sealed: SealedHelper
    trait SealedHelper {
      def apply(): Tree
      def unapply(tree: Tree): Boolean
    }

    val Override: OverrideHelper
    trait OverrideHelper {
      def apply(): Tree
      def unapply(tree: Tree): Boolean
    }

    val Abstract: AbstractHelper
    trait AbstractHelper {
      def apply(): Tree
      def unapply(tree: Tree): Boolean
    }

    val Lazy: LazyHelper
    trait LazyHelper {
      def apply(): Tree
      def unapply(tree: Tree): Boolean
    }

    val Inline: InlineHelper
    trait InlineHelper {
      def apply(): Tree
      def unapply(tree: Tree): Boolean
    }

    val Type: TypeHelper
    trait TypeHelper {
      def apply(): Tree
      def unapply(tree: Tree): Boolean
    }

    val Case: CaseHelper
    trait CaseHelper {
      def apply(): Tree
      def unapply(tree: Tree): Boolean
    }

    val Contravariant: ContravariantHelper
    trait ContravariantHelper {
      def apply(): Tree
      def unapply(tree: Tree): Boolean
    }

    val Covariant: CovariantHelper
    trait CovariantHelper {
      def apply(): Tree
      def unapply(tree: Tree): Boolean
    }

    val Annot: AnnotHelper
    trait AnnotHelper {
      def apply(body: Tree): Tree
      def unapply(tree: Tree): Option[Tree]
    }
  }
}

