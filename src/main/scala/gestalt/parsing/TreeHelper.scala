package scala.gestalt
package parsing

import java.util.IdentityHashMap

trait TreeHelper {
  val tb: StructToolbox
  val tbName: String

  import tb._

  private implicit class TreeOps(val tree: Tree) {
    def select(name: String): Tree = Select(tree, name)

    def appliedTo(args: Tree*): Tree = Apply(tree, args.toList)

    def appliedToType(args: TypeTree*): Tree = ApplyType(tree, args.toList)
  }

  // sometimes, the parser needs to check the unlifted Tree
  val biMap = new IdentityHashMap[Tree, Tree]
  def unlift(tree: AnyRef): Tree = biMap.get(tree)
  def unliftTypeTree(tree: AnyRef): TypeTree = biMap.get(tree).asInstanceOf[TypeTree]
  def lift(tree: Tree): Tree = biMap.get(tree)
  def map(lifted: Tree, unlifted: Tree): Unit = {
    biMap.put(lifted, unlifted)
    biMap.put(unlifted, lifted)
  }

  // lifted trees
  lazy val scalaNil = select("scala.Nil")
  lazy val scalaList = select("scala.List")
  lazy val scalaSome = select("scala.Some")
  lazy val scalaNone = select("scala.None")
  lazy val toolbox = Ident(tbName)
  lazy val root = Ident("_root_")

  private def select(path: String, isTerm: Boolean = true): Tree = {
    val parts = path.split('.')

    val qual = parts.init.foldLeft[Tree](root) { (prefix, name) =>
      prefix.select(name)
    }

    if (isTerm) Select(qual, parts.last) else TypeSelect(qual, parts.last)
  }

  /* -------------------- lifting definitions -------------------------- */
  def liftMods(mods: Mods): Tree = ???

  def liftFunctionParam(name: String, tpe: Tree): Tree = {
    val rawTpe = if (tpe == null) None else Some(unliftTypeTree(tpe))
    val raw = Param(emptyMods, name, rawTpe, None)

    val liftedTpe = if (tpe == null) scalaNone else scalaSome.appliedTo(tpe)
    val lifted = toolbox.select("Param").appliedTo(toolbox.select("emptyMods"), Lit(name), liftedTpe, scalaNone)

    map(lifted, raw)
    lifted
  }

  def liftParam(mods: Mods, name: String, tpe: Tree, default: Tree): Tree = ???


  def liftSelf(name: String, tpe: Tree): Tree = ???
  def liftSelf(name: String): Tree = ???

  def liftInitCall(qual: Tree, name: String, tparams: Seq[Tree], argss: Seq[Seq[Tree]]): Tree = ???

  def liftTypeParam(mods: Mods, name: String, tparams: Seq[Tree], tbounds: Tree, cbounds: Seq[Tree]): Tree = ???

  def liftValDef(mods: Mods, name: String, tpe: Tree, rhs: Tree): Tree = ???
  def liftPatDef(mods: Mods, lhs: Tree, tpe: Tree, rhs: Tree): Tree = ???
  def liftSeqDef(mods: Mods, pats: Seq[Tree], tpe: Tree, rhs: Tree): Tree = ???
  def liftValDecl(mods: Mods, name: String, tpe: Tree): Tree = ???
  def liftSeqDecl(mods: Mods, vals: Seq[String], tpe: Tree): Tree = ???

  def liftDefDef(mods: Mods, name: String, tparams: Seq[Tree], paramss: Seq[Seq[Tree]], tpe: Tree, rhs: Tree): Tree = ???
  def liftDefDecl(mods: Mods, name: String, tparams: Seq[Tree], paramss: Seq[Seq[Tree]], tpe: Tree): Tree = ???

  def liftObject(mods: Mods, name: String, parents: Seq[Tree], self: Tree, stats: Seq[Tree]): Tree = ???
  def liftClass(mods: Mods, name: String, tparams: Seq[Tree], ctor: Tree, parents: Seq[Tree], self: Tree, stats: Seq[Tree]): Tree = ???
  def liftAnonymClass(parents: Seq[Tree], self: Tree, stats: Seq[Tree]): Tree = ???
  def liftTrait(mods: Mods, name: String, tparams: Seq[Tree], ctor: Tree, parents: Seq[Tree], self: Tree, stats: Seq[Tree]): Tree = ???
  def liftTypeDecl(mods: Mods, name: String, tparams: Seq[Tree], tbounds: Tree): Tree = ???
  def liftTypeAlias(mods: Mods, name: String, tparams: Seq[Tree], rhs: Tree): Tree = ???

  def liftPrimaryCtor(mods: Mods, paramss: Seq[Seq[Tree]]): Tree = ???
  def liftSecondaryCtor(mods: Mods, paramss: Seq[Seq[Tree]], rhs: Tree): Tree = ???

  /* -------------------- lifting terms -------------------------- */
  def liftLit(value: Any): Tree = {
    val raw = Lit(value)
    val lifted = toolbox.select("Lit").appliedTo(Lit(value))
    map(lifted, raw)
    lifted
  }

  def liftIdent(name: String): Tree = {
    val raw = Ident(name)
    val lifted = toolbox.select("Ident").appliedTo(Lit(name))
    map(lifted, raw)
    lifted
  }

  def liftSelect(qual: Tree, name: String): Tree = {
    val raw = Select(unlift(qual), name)
    val lifted = toolbox.select("Select").appliedTo(qual, Lit(name))
    map(lifted, raw)
    lifted
  }

  def liftApply(fun: Tree, args: Seq[Tree]): Tree = ???

  def liftApplyType(fun: Tree, args: Seq[Tree]): Tree = ???

  def liftThis(qual: String): Tree = {
    val raw = This(qual)
    val lifted = toolbox.select("This").appliedTo(Lit(qual))
    map(lifted, raw)
    lifted
  }

  def liftSuper(thisp: String, superp: String): Tree = {
    val raw = Super(thisp, superp)
    val lifted = toolbox.select("This").appliedTo(Lit(thisp), Lit(superp))
    map(lifted, raw)
    lifted
  }

  def liftInfix(lhs: Tree, op: String, rhs: Tree): Tree = {
    val raw = Infix(unlift(lhs), op, rhs)
    val lifted = toolbox.select("Infix").appliedTo(lhs, liftLit(op), rhs)
    map(lifted, raw)
    lifted
  }

  def liftPostfix(od: Tree, op: String): Tree = {
    val raw = Postfix(unlift(od), op)
    val lifted = toolbox.select("Postfix").appliedTo(od, liftIdent(op))
    map(lifted, raw)
    lifted
  }

  def liftPrefix(op: String, od: Tree): Tree = {
    val raw = Prefix(op, unlift(od))
    val lifted = toolbox.select("Prefix").appliedTo(liftIdent(op), od)
    map(lifted, raw)
    lifted
  }

  def liftBlock(trees: Seq[Tree]): Tree = {
    val raw = Block(trees.map(unlift))
    val args = trees.foldLeft(scalaNil) { (acc, tree) => Infix(tree, "::", acc) }
    val lifted = toolbox.select("Block").appliedTo(args)
    map(lifted, raw)
    lifted
  }

  def liftTuple(trees: Seq[Tree]): Tree = {
    val raw = Tuple(trees.map(unlift))
    val args = trees.foldLeft(scalaNil) { (acc, tree) => Infix(tree, "::", acc) }
    val lifted = toolbox.select("Tuple").appliedTo(args)
    map(lifted, raw)
    lifted
  }

  def liftInterpolate(tag: String, parts: Seq[String], args: Seq[Tree]): Tree = {
    val raw = Interpolate(tag, parts, args.map(unlift))
    val liftedParts = parts.foldLeft(scalaNil) { (acc, str) => Infix(Lit(str), "::", acc) }
    val liftedArgs = args.foldLeft(scalaNil) { (acc, tree) => Infix(tree, "::", acc) }
    val lifted = toolbox.select("Interpolate").appliedTo(Lit(tag), liftedParts, liftedArgs)
    map(lifted, raw)
    lifted
  }

  def liftIf(cond: Tree, thenp: Tree, elsep: Tree): Tree = {
    val elsep1 = if (elsep == null) None else Some(unlift(elsep))
    val raw = If(unlift(cond), unlift(thenp), elsep1)
    val liftedElsep = if (elsep == null) scalaNone else scalaSome.appliedTo(elsep)
    val lifted = toolbox.select("If").appliedTo(cond, thenp, liftedElsep)
    map(lifted, raw)
    lifted
  }

  def liftWhile(expr: Tree, body: Tree): Tree = {
    val raw = While(unlift(expr), unlift(body))
    val lifted = toolbox.select("While").appliedTo(expr, body)
    map(lifted, raw)
    lifted
  }

  def liftDoWhile(body: Tree, expr: Tree): Tree = {
    val raw = DoWhile(unlift(body), unlift(expr))
    val lifted = toolbox.select("DoWhile").appliedTo(expr, body)
    map(lifted, raw)
    lifted
  }

  def liftTry(body: Tree, cases: Seq[Tree], finalizer: Tree): Tree = {
    val raw =
      Try(
        unlift(body),
        cases.map(unlift),
        if (finalizer == null) None else Some(unlift(finalizer))
      )

    val liftedCases = cases.foldLeft(scalaNil) { (acc, tree) => Infix(tree, "::", acc) }
    val lifted = toolbox.select("Try").appliedTo(body, liftedCases, finalizer)
    map(lifted, raw)
    lifted
  }

  def liftTry(body: Tree, handler: Tree, finalizer: Tree): Tree = {
    val raw =
      Try(
        unlift(body),
        unlift(handler),
        if (finalizer == null) None else Some(unlift(finalizer))
      )

    val lifted = toolbox.select("Try").appliedTo(body, handler, finalizer)
    map(lifted, raw)
    lifted
  }

  def liftThrow(expr: Tree): Tree = {
    val raw = Throw(unlift(expr))
    val lifted = toolbox.select("Throw").appliedTo(expr)
    map(lifted, raw)
    lifted
  }

  def liftReturn(expr: Tree): Tree = {
    val raw = Return(unlift(expr))
    val lifted = toolbox.select("Return").appliedTo(expr)
    map(lifted, raw)
    lifted
  }

  def liftReturn: Tree = {
    val raw = Return
    val lifted = toolbox.select("Return")
    map(lifted, raw)
    lifted
  }

  def liftMatch(expr: Tree, cases: Seq[Tree]): Tree = {
    val raw = Match(unlift(expr), cases.map(unlift))
    val liftedCases = cases.foldLeft(scalaNil) { (acc, tree) => Infix(tree, "::", acc) }
    val lifted = toolbox.select("Match").appliedTo(expr, liftedCases)
    map(lifted, raw)
    lifted
  }

  def liftCase(pat: Tree, cond: Tree, body: Tree): Tree = ???

  def liftAscribe(expr: Tree, tpe: Tree): Tree = {
    val raw = Ascribe(unlift(expr), unlift(tpe))
    val lifted = toolbox.select("Ascribe").appliedTo(expr, tpe)
    map(lifted, raw)
    lifted
  }

  def liftAnnotated(expr: Tree, annots: Seq[Tree]): Tree = {
    val raw = Annotated(unlift(expr), annots.map(unlift))
    val liftedAnnots = annots.foldLeft(scalaNil) { (acc, tree) => Infix(tree, "::", acc) }
    val lifted = toolbox.select("Annotated").appliedTo(expr, liftedAnnots)
    map(lifted, raw)
    lifted
  }

  def liftFunction(params: Seq[Tree], body: Tree): Tree = {
    val raw = Function(params.map(unlift), body)
    val liftedParams = params.foldLeft(scalaNil) { (acc, tree) => Infix(tree, "::", acc) }
    val lifted = toolbox.select("Function").appliedTo(liftedParams, body)
    map(lifted, raw)
    lifted
  }

  def liftNew(tpe: Tree): Tree = ???
  def liftNamed(name: String, expr: Tree): Tree = ???


  def liftFor(enums: Seq[Tree], body: Tree): Tree = ???
  def liftGenFrom(pat: Tree, rhs: Tree): Tree = ???
  def liftGenAlias(pat: Tree, rhs: Tree): Tree = ???
  def liftGuard(cond: Tree): Tree = ???
  def liftYield(expr: Tree): Tree = ???

  def liftBind(name: String, expr: Tree): Tree = ???
  def liftAlternative(trees: Seq[Tree]): Tree = ???

  /* -------------------- importing  ----------------------------- */
  def liftImport(items: Seq[Tree]): Tree = ???
  def liftImportItem(ref: Tree, importees: Seq[Tree]): Tree = ???
  def liftImportName(name: String): Tree = ???
  def liftImportRename(from: String, to: String): Tree = ???
  def liftImportHide(name: String): Tree = ???

  /* -------------------- lifting types -------------------------- */
  def liftTypeIdent(name: String): Tree = {
    val raw = TypeIdent(name)
    val lifted = toolbox.select("TypeIdent").appliedTo(Lit(name))
    map(lifted, raw)
    lifted
  }

  def liftTypeSelect(qual: Tree, name: String): Tree = {
    val raw = TypeSelect(unliftTypeTree(qual), name)
    val lifted = toolbox.select("TypeSelect").appliedTo(qual, Lit(name))
    map(lifted, raw)
    lifted
  }

  def liftTypeApplyInfix(lhs: Tree, op: String, rhs: Tree): Tree = {
    val raw = TypeApplyInfix(unliftTypeTree(lhs), op, unliftTypeTree(rhs))
    val lifted = toolbox.select("TypeApplyInfix").appliedTo(lhs, liftLit(op), rhs)
    map(lifted, raw)
    lifted
  }

  def liftTypeFunction(params: Seq[Tree], res: Tree): Tree = {
    val raw = TypeFunction(params.map(unliftTypeTree), unliftTypeTree(res))
    val liftedParams = params.foldLeft(scalaNil) { (acc, tree) => Infix(tree, "::", acc) }
    val lifted = toolbox.select("TypeFunction").appliedTo(liftedParams, res)
    map(lifted, raw)
    lifted
  }

  def liftTypeRefine(tpe: Tree, stats: Seq[Tree]) = {
    val raw = TypeRefine(if (tpe == null) None else Some(unliftTypeTree(tpe)), stats.map(unliftTypeTree))
    val liftedTpe = if (tpe == null) scalaNone else scalaSome.appliedTo(tpe)
    val liftedStats = stats.foldLeft(scalaNil) { (acc, tree) => Infix(tree, "::", acc) }
    val lifted = toolbox.select("TypeRefine").appliedTo(liftedTpe, liftedStats)
    map(lifted, raw)
    lifted
  }

  def liftTypeAnd(lhs: Tree, rhs: Tree): Tree = {
    val raw = TypeAnd(unliftTypeTree(lhs), unliftTypeTree(rhs))
    val lifted = toolbox.select("TypeAnd").appliedTo(lhs, rhs)
    map(lifted, raw)
    lifted
  }

  def liftTypeOr(lhs: Tree, rhs: Tree): Tree = {
    val raw = TypeOr(unliftTypeTree(lhs), unliftTypeTree(rhs))
    val lifted = toolbox.select("TypeOr").appliedTo(lhs, rhs)
    map(lifted, raw)
    lifted
  }

  def liftTypeAnnotated(tpe: Tree, annots: Seq[Tree]): Tree = {
    val raw = TypeAnnotated(unliftTypeTree(tpe), annots.map(unliftTypeTree))
    val liftedAnnots = annots.foldLeft(scalaNil) { (acc, tree) => Infix(tree, "::", acc) }
    val lifted = toolbox.select("TypeAnnotated").appliedTo(tpe, liftedAnnots)
    map(lifted, raw)
    lifted
  }

  def liftTypeSingleton(ref: Tree): Tree = {
    val raw = TypeSingleton(unliftTypeTree(ref))
    val lifted = toolbox.select("TypeSingleton").appliedTo(ref)
    map(lifted, raw)
    lifted
  }

  def liftTypeApply(tpe: Tree, args: Seq[Tree]): Tree = {
    val raw = TypeApply(unliftTypeTree(tpe), args.map(unliftTypeTree))
    val liftedArgs = args.foldLeft(scalaNil) { (acc, tree) => Infix(tree, "::", acc) }
    val lifted = toolbox.select("TypeApply").appliedTo(tpe, liftedArgs)
    map(lifted, raw)
    lifted
  }

  def liftTypeByName(tpe: Tree): Tree = {
    val raw = TypeByName(unliftTypeTree(tpe))
    val lifted = toolbox.select("TypeByName").appliedTo(tpe)
    map(lifted, raw)
    lifted
  }

  def liftTypeRepeated(tpe: Tree): Tree = {
    val raw = TypeRepeated(unliftTypeTree(tpe))
    val lifted = toolbox.select("TypeRepeated").appliedTo(tpe)
    map(lifted, raw)
    lifted
  }

  def liftTypeBounds(lo: Tree, hi: Tree): Tree = {
    val raw = TypeBounds(
      if (lo == null) None else Some(unliftTypeTree(lo)),
      if (hi == null) None else Some(unliftTypeTree(hi))
    )
    val liftedLO = if (lo == null) scalaNone else scalaSome.appliedTo(lo)
    val liftedHI = if (hi == null) scalaNone else scalaSome.appliedTo(hi)
    val lifted = toolbox.select("TypeBounds").appliedTo(liftedLO, liftedHI)
    map(lifted, raw)
    lifted
  }
}

