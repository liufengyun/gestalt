package scala.gestalt
package parsing

import java.util.IdentityHashMap

trait TreeHelper {
  val tb: StructToolbox
  val tbName: String

  import tb._

  private implicit class TreeOps(val tree: TermTree) {
    def select(name: String): TermTree = Select(tree, name)

    def appliedTo(args: TermTree*): TermTree = Apply(tree, args.toList)
  }

  // sometimes, the parser needs to check the unlifted Tree
  val biMap = new IdentityHashMap[Tree, Tree]
  def unlift(tree: AnyRef): Tree = biMap.get(tree)
  def unliftAs[T <: Tree](tree: AnyRef): T = biMap.get(tree).asInstanceOf[T]
  def lift(tree: Tree): TermTree = biMap.get(tree).asInstanceOf[TermTree]
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

  private def select(path: String): TermTree = {
    val parts = path.split('.')

    parts.foldLeft[TermTree](root) { (prefix, name) =>
      prefix.select(name)
    }
  }

  /* -------------------- lifting helpers -------------------------- */
  def liftSeqSeq(trees: Seq[Seq[TermTree]]): TermTree =
    trees.foldLeft(scalaNil) { (acc, tree) => Infix(liftSeq(tree), "::", acc) }

  def liftSeq(trees: Seq[TermTree]): TermTree =
    trees.foldLeft(scalaNil) { (acc, tree) => Infix(tree, "::", acc) }

  def liftOption(tree: TermTree): TermTree =
    if (tree == null) scalaNone else scalaSome.appliedTo(tree)

  def liftMods(mods: Mods): TermTree = {
    var lifted = toolbox.select("emptyMods")

    if (mods.isPrivate) lifted = lifted.select("setPrivate").appliedTo(Lit(mods.privateWithin))
    if (mods.isProtected) lifted = lifted.select("setProtected").appliedTo(Lit(mods.privateWithin))
    if (mods.isOverride) lifted = lifted.select("setOverride")
    if (mods.isFinal) lifted = lifted.select("setFinal")
    if (mods.isImplicit) lifted = lifted.select("setImplicit")
    if (mods.isLazy) lifted = lifted.select("setLazy")
    if (mods.isSealed) lifted = lifted.select("setSealed")
    if (mods.isAbstract) lifted = lifted.select("setAbstract")
    if (mods.isValParam) lifted = lifted.select("setValParam")
    if (mods.isVarParam) lifted = lifted.select("setVarParam")
    if (mods.isCase) lifted = lifted.select("setCase")
    if (mods.isContravariant) lifted = lifted.select("setContravairnat")
    if (mods.isCovariant) lifted = lifted.select("setCovariant")
    if (mods.isInline) lifted = lifted.select("setInline")
    if (mods.isMutable) lifted = lifted.select("setMutable")

    if (mods.hasAnnotations)
      lifted = lifted.select("withAnnotations").appliedTo(liftSeq(mods.annotations.asInstanceOf[Seq[TermTree]]))

    lifted
  }
  /* -------------------- lifting definitions -------------------------- */

  def liftFunctionParam(name: String, tpe: TermTree): TermTree = {
    val rawTpe = if (tpe == null) None else Some(unliftAs[TypeTree](tpe))
    val raw = Param(emptyMods, name, rawTpe, None)

    val liftedTpe = if (tpe == null) scalaNone else scalaSome.appliedTo(tpe)
    val lifted = toolbox.select("Param").appliedTo(toolbox.select("emptyMods"), Lit(name), liftedTpe, scalaNone)

    map(lifted, raw)
    lifted
  }

  def liftParam(mods: Mods, name: String, tpe: TermTree, default: TermTree): TermTree = {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))
    val rawTpe = if (tpe == null) None else Some(unliftAs[TypeTree](tpe))
    val rawDefault = if (default == null) None else Some(unliftAs[TermTree](default))
    val raw = Param(rawMods, name, rawTpe, rawDefault)
    val lifted = toolbox.select("Param").appliedTo(liftMods(mods), Lit(name), liftOption(tpe), liftOption(default))

    map(lifted, raw)
    lifted
  }

  def liftSelf(name: String, tpe: TermTree): TermTree = {
    val raw = Self(name, unliftAs[TypeTree](tpe))
    val lifted = toolbox.select("Self").appliedTo(Lit(name), tpe)
    map(lifted, raw)
    lifted
  }

  def liftSelf(name: String): TermTree = {
    val raw = Self(name)
    val lifted = toolbox.select("Self").appliedTo(Lit(name))
    map(lifted, raw)
    lifted
  }

  def liftInitCall(qual: TermTree, name: String, targs: Seq[TermTree], argss: Seq[Seq[TermTree]]): TermTree = {
    val rawQual = if (qual == null) None else Some(unlift(qual))
    val rawArgss = argss.map(_.map(unliftAs[TermTree]))
    val raw = InitCall(rawQual, name, targs.map(unliftAs[TypeTree]), rawArgss)
    val lifted = toolbox.select("InitCall").appliedTo(
      liftOption(qual),
      Lit(name),
      liftSeq(targs),
      liftSeqSeq(argss)
    )
    map(lifted, raw)
    lifted
  }

  def liftTypeParam(mods: Mods, name: String, tparams: Seq[TermTree],
                    tbounds: TermTree, cbounds: Seq[TermTree]
                   ): TermTree =
  {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))
    val rawBounds = if (tbounds == null) None else Some(unliftAs[TypeTree](tbounds))
    val raw = TypeParam(
      rawMods, name, tparams.map(unliftAs[TypeParam]), rawBounds, cbounds.map(unliftAs[TypeTree])
    )
    val lifted = toolbox.select("TypeParam").appliedTo(
      liftMods(mods), Lit(name), liftSeq(tparams), liftOption(tbounds), liftSeq(cbounds)
    )

    map(lifted, raw)
    lifted
  }

  def liftValDef(mods: Mods, name: String, tpe: TermTree, rhs: TermTree): TermTree = {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))
    val rawTpe = if (tpe == null) None else Some(unliftAs[TypeTree](tpe))
    val raw = ValDef(rawMods, name, rawTpe, unliftAs[TermTree](rhs))
    val lifted = toolbox.select("ValDef").appliedTo(
      liftMods(mods), Lit(name), liftOption(tpe), rhs
    )

    map(lifted, raw)
    lifted
  }

  def liftPatDef(mods: Mods, lhs: TermTree, tpe: TermTree, rhs: TermTree): TermTree = {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))
    val rawTpe = if (tpe == null) None else Some(unliftAs[TypeTree](tpe))
    val raw = PatDef(rawMods, unliftAs[TermTree](lhs), rawTpe, unliftAs[TermTree](rhs))
    val lifted = toolbox.select("PatDef").appliedTo(
      liftMods(mods), lhs, liftOption(tpe), rhs
    )

    map(lifted, raw)
    lifted
  }

  def liftSeqDef(mods: Mods, pats: Seq[TermTree], tpe: TermTree, rhs: TermTree): TermTree = {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))
    val rawTpe = if (tpe == null) None else Some(unliftAs[TypeTree](tpe))
    val raw = SeqDef(rawMods, pats.map(unliftAs[TermTree]), rawTpe, unliftAs[TermTree](rhs))
    val lifted = toolbox.select("SeqDef").appliedTo(
      liftMods(mods), liftSeq(pats), liftOption(tpe), rhs
    )

    map(lifted, raw)
    lifted
  }

  def liftValDecl(mods: Mods, name: String, tpe: TermTree): TermTree = {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))
    val raw = ValDecl(rawMods, name, unliftAs[TypeTree](tpe))
    val lifted = toolbox.select("ValDecl").appliedTo(liftMods(mods), Lit(name), tpe)

    map(lifted, raw)
    lifted
  }

  def liftSeqDecl(mods: Mods, vals: Seq[String], tpe: TermTree): TermTree = {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))
    val raw = SeqDecl(rawMods, vals, unliftAs[TypeTree](tpe))
    val liftedNames = vals.foldLeft(scalaNil) { (acc, name) => Infix(Lit(name), "::", acc) }
    val lifted = toolbox.select("SeqDecl").appliedTo(liftMods(mods), liftedNames, tpe)

    map(lifted, raw)
    lifted
  }

  def liftDefDef(mods: Mods, name: String, tparams: Seq[TermTree],
                 paramss: Seq[Seq[TermTree]], tpe: TermTree, rhs: TermTree
                ): TermTree =
  {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))
    val rawTpe = if (tpe == null) None else Some(unliftAs[TypeTree](tpe))
    val raw = DefDef(
      rawMods, name, tparams.map(unliftAs[TypeParam]),
      paramss.map(_.map(unliftAs[Param])),
      rawTpe, unliftAs[TermTree](rhs)
    )
    val lifted = toolbox.select("DefDef").appliedTo(
      liftMods(mods), Lit(name), liftSeq(tparams), liftSeqSeq(paramss), liftOption(tpe), rhs
    )

    map(lifted, raw)
    lifted
  }

  def liftDefDecl(mods: Mods, name: String, tparams: Seq[TermTree], paramss: Seq[Seq[TermTree]], tpe: TermTree): TermTree = {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))
    val raw = DefDecl(
      rawMods, name, tparams.map(unliftAs[TypeParam]),
      paramss.map(_.map(unliftAs[Param])),
      unliftAs[TypeTree](tpe)
    )
    val lifted = toolbox.select("DefDef").appliedTo(
      liftMods(mods), Lit(name), liftSeq(tparams), liftSeqSeq(paramss), tpe
    )

    map(lifted, raw)
    lifted
  }

  def liftObject(mods: Mods, name: String, parents: Seq[TermTree], self: TermTree, stats: Seq[TermTree]): TermTree = {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))
    val rawSelf = if (self == null) None else Some(unliftAs[Self](self))
    val raw = Object(
      rawMods, name, parents.map(unliftAs[InitCall]), rawSelf, stats.map(unlift)
    )

    val lifted = toolbox.select("Object").appliedTo(
      liftMods(mods), Lit(name), liftSeq(parents), liftOption(self), liftSeq(stats)
    )

    map(lifted, raw)
    lifted
  }

  def liftClass(mods: Mods, name: String, tparams: Seq[TermTree], cmods: Mods, paramss: Seq[Seq[TermTree]],
                parents: Seq[TermTree], self: TermTree, stats: Seq[TermTree]
               ): TermTree =
  {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))
    val rawcMods = cmods.withAnnotations(cmods.annotations.map(unlift))
    val rawSelf = if (self == null) None else Some(unliftAs[Self](self))
    val raw = Class(
      rawMods, name, tparams.map(unliftAs[TypeParam]), rawcMods, paramss.map(_.map(unliftAs[Param])),
      parents.map(unliftAs[InitCall]), rawSelf, stats.map(unlift)
    )

    val lifted = toolbox.select("Class").appliedTo(
      liftMods(mods), Lit(name), liftSeq(tparams), liftMods(cmods),
      liftSeqSeq(paramss), liftSeq(parents), liftOption(self), liftSeq(stats)
    )

    map(lifted, raw)
    lifted
  }

  def liftAnonymClass(parents: Seq[TermTree], self: TermTree, stats: Seq[TermTree]): TermTree = {
    val rawSelf = if (self == null) None else Some(unliftAs[Self](self))
    val raw = AnonymClass(parents.map(unliftAs[InitCall]), rawSelf, stats.map(unlift))

    val lifted = toolbox.select("AnonymClass").appliedTo(liftSeq(parents), liftOption(self), liftSeq(stats))

    map(lifted, raw)
    lifted
  }

  def liftTrait(mods: Mods, name: String, tparams: Seq[TermTree], cmods: Mods, paramss: Seq[Seq[TermTree]],
                parents: Seq[TermTree], self: TermTree, stats: Seq[TermTree]): TermTree =
  {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))
    val rawcMods = cmods.withAnnotations(cmods.annotations.map(unlift))
    val rawSelf = if (self == null) None else Some(unliftAs[Self](self))
    val raw = Trait(
      rawMods, name, tparams.map(unliftAs[TypeParam]), rawcMods, paramss.map(_.map(unliftAs[Param])),
      parents.map(unliftAs[InitCall]), rawSelf, stats.map(unlift)
    )

    val lifted = toolbox.select("Trait").appliedTo(
      liftMods(mods), Lit(name), liftSeq(tparams), liftMods(cmods),
      liftSeqSeq(paramss), liftSeq(parents), liftOption(self), liftSeq(stats)
    )

    map(lifted, raw)
    lifted
  }

  def liftTypeDecl(mods: Mods, name: String, tparams: Seq[TermTree], tbounds: TermTree): TermTree = {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))
    val rawBounds = if (tbounds == null) None else Some(unliftAs[TypeTree](tbounds))
    val raw = TypeDecl(rawMods, name, tparams.map(unliftAs[TypeParam]), rawBounds)
    val lifted = toolbox.select("TypeDecl").appliedTo(
      liftMods(mods), Lit(name), liftSeq(tparams), liftOption(tbounds)
    )

    map(lifted, raw)
    lifted
  }

  def liftTypeAlias(mods: Mods, name: String, tparams: Seq[TermTree], rhs: TermTree): TermTree = {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))
    val raw = TypeAlias(rawMods, name, tparams.map(unliftAs[TypeParam]), unliftAs[TypeTree](rhs))
    val lifted = toolbox.select("TypeAlias").appliedTo(liftMods(mods), Lit(name), liftSeq(tparams), rhs)

    map(lifted, raw)
    lifted
  }

  def liftSecondaryCtor(mods: Mods, paramss: Seq[Seq[TermTree]], rhs: TermTree): TermTree = {
    val rawMods = mods.withAnnotations(mods.annotations.map(unlift))

    val raw = SecondaryCtor(rawMods, paramss.map(_.map(unliftAs[Param])), unliftAs[TermTree](rhs))
    val lifted = toolbox.select("SecondaryCtor").appliedTo(liftMods(mods), liftSeqSeq(paramss), rhs)

    map(lifted, raw)
    lifted
  }

  /* -------------------- lifting terms -------------------------- */
  def liftLit(value: Any): TermTree = {
    val raw = Lit(value)
    val lifted = toolbox.select("Lit").appliedTo(Lit(value))
    map(lifted, raw)
    lifted
  }

  def liftIdent(name: String): TermTree = {
    val raw = Ident(name)
    val lifted = toolbox.select("Ident").appliedTo(Lit(name))
    map(lifted, raw)
    lifted
  }

  def liftSelect(qual: TermTree, name: String): TermTree = {
    val raw = Select(unliftAs[TermTree](qual), name)
    val lifted = toolbox.select("Select").appliedTo(qual, Lit(name))
    map(lifted, raw)
    lifted
  }

  def liftApply(fun: TermTree, args: Seq[TermTree]): TermTree = {
    val raw = Apply(unliftAs[TermTree](fun), args.map(unliftAs[TermTree](_)))
    val lifted = toolbox.select("Apply").appliedTo(fun, liftSeq(args))
    map(lifted, raw)
    lifted
  }

  def liftApplyType(fun: TermTree, args: Seq[TermTree]): TermTree = {
    val raw = ApplyType(unliftAs[TermTree](fun), args.map(unliftAs[TypeTree](_)))
    val lifted = toolbox.select("ApplyType").appliedTo(fun, liftSeq(args))
    map(lifted, raw)
    lifted
  }

  def liftThis(qual: String): TermTree = {
    val raw = This(qual)
    val lifted = toolbox.select("This").appliedTo(Lit(qual))
    map(lifted, raw)
    lifted
  }

  def liftSuper(thisp: String, superp: String): TermTree = {
    val raw = Super(thisp, superp)
    val lifted = toolbox.select("This").appliedTo(Lit(thisp), Lit(superp))
    map(lifted, raw)
    lifted
  }

  def liftInfix(lhs: TermTree, op: String, rhs: TermTree): TermTree = {
    val raw = Infix(unliftAs[TermTree](lhs), op, unliftAs[TermTree](rhs))
    val lifted = toolbox.select("Infix").appliedTo(lhs, Lit(op), rhs)
    map(lifted, raw)
    lifted
  }

  def liftAssign(lhs: TermTree, rhs: TermTree): TermTree = {
    val raw = Assign(unliftAs[TermTree](lhs), unliftAs[TermTree](rhs))
    val lifted = toolbox.select("Assign").appliedTo(lhs, rhs)
    map(lifted, raw)
    lifted
  }

  def liftPostfix(od: TermTree, op: String): TermTree = {
    val raw = Postfix(unliftAs[TermTree](od), op)
    val lifted = toolbox.select("Postfix").appliedTo(od, Lit(op))
    map(lifted, raw)
    lifted
  }

  def liftPrefix(op: String, od: TermTree): TermTree = {
    val raw = Prefix(op, unliftAs[TermTree](od))
    val lifted = toolbox.select("Prefix").appliedTo(Lit(op), od)
    map(lifted, raw)
    lifted
  }

  def liftBlock(trees: Seq[TermTree]): TermTree = {
    val raw = Block(trees.map(unlift))
    val lifted = toolbox.select("Block").appliedTo(liftSeq(trees))
    map(lifted, raw)
    lifted
  }

  def liftTuple(trees: Seq[TermTree]): TermTree = {
    val raw = Tuple(trees.map(unliftAs[TermTree]))
    val lifted = toolbox.select("Tuple").appliedTo(liftSeq(trees))
    map(lifted, raw)
    lifted
  }

  def liftInterpolate(tag: String, parts: Seq[String], args: Seq[TermTree]): TermTree = {
    val raw = Interpolate(tag, parts, args.map(unliftAs[TermTree]))
    val liftedParts = parts.foldLeft(scalaNil) { (acc, name) => Infix(Lit(name), "::", acc) }
    val liftedArgs = liftSeq(args)
    val lifted = toolbox.select("Interpolate").appliedTo(Lit(tag), liftedParts, liftedArgs)
    map(lifted, raw)
    lifted
  }

  def liftIf(cond: TermTree, thenp: TermTree, elsep: TermTree): TermTree = {
    val elsep1 = if (elsep == null) None else Some(unliftAs[TermTree](elsep))
    val raw = If(unliftAs[TermTree](cond), unliftAs[TermTree](thenp), elsep1)
    val liftedElsep = liftOption(elsep)
    val lifted = toolbox.select("If").appliedTo(cond, thenp, liftedElsep)
    map(lifted, raw)
    lifted
  }

  def liftWhile(expr: TermTree, body: TermTree): TermTree = {
    val raw = While(unliftAs[TermTree](expr), unliftAs[TermTree](body))
    val lifted = toolbox.select("While").appliedTo(expr, body)
    map(lifted, raw)
    lifted
  }

  def liftDoWhile(body: TermTree, expr: TermTree): TermTree = {
    val raw = DoWhile(unliftAs[TermTree](body), unliftAs[TermTree](expr))
    val lifted = toolbox.select("DoWhile").appliedTo(expr, body)
    map(lifted, raw)
    lifted
  }

  def liftTry(body: TermTree, cases: Seq[TermTree], finalizer: TermTree): TermTree = {
    val raw =
      Try(
        unliftAs[TermTree](body),
        cases.map(unlift),
        if (finalizer == null) None else Some(unliftAs[TermTree](finalizer))
      )

    val liftedCases = liftSeq(cases)
    val lifted = toolbox.select("Try").appliedTo(body, liftedCases, finalizer)
    map(lifted, raw)
    lifted
  }

  def liftTry(body: TermTree, handler: TermTree, finalizer: TermTree): TermTree = {
    val raw =
      Try(
        unliftAs[TermTree](body),
        unlift(handler),
        if (finalizer == null) None else Some(unliftAs[TermTree](finalizer))
      )

    val lifted = toolbox.select("Try").appliedTo(body, handler, finalizer)
    map(lifted, raw)
    lifted
  }

  def liftThrow(expr: TermTree): TermTree = {
    val raw = Throw(unliftAs[TermTree](expr))
    val lifted = toolbox.select("Throw").appliedTo(expr)
    map(lifted, raw)
    lifted
  }

  def liftReturn(expr: TermTree): TermTree = {
    val raw = Return(unliftAs[TermTree](expr))
    val lifted = toolbox.select("Return").appliedTo(expr)
    map(lifted, raw)
    lifted
  }

  def liftReturn: TermTree = {
    val raw = Return
    val lifted = toolbox.select("Return")
    map(lifted, raw)
    lifted
  }

  def liftMatch(expr: TermTree, cases: Seq[TermTree]): TermTree = {
    val raw = Match(unliftAs[TermTree](expr), cases.map(unlift))
    val liftedCases = cases.foldLeft(scalaNil) { (acc, tree) => Infix(tree, "::", acc) }
    val lifted = toolbox.select("Match").appliedTo(expr, liftedCases)
    map(lifted, raw)
    lifted
  }

  def liftCase(pat: TermTree, cond: TermTree, body: TermTree): TermTree = {
    val rawCond = if (cond == null) None else Some(unliftAs[TermTree](cond))
    val raw = Case(unliftAs[TermTree](pat), rawCond, unliftAs[TermTree](body))
    val lifted = toolbox.select("Case").appliedTo(pat, liftOption(cond), body)
    map(lifted, raw)
    lifted
  }

  def liftAscribe(expr: TermTree, tpe: TermTree): TermTree = {
    val raw = Ascribe(unliftAs[TermTree](expr), unliftAs[TypeTree](tpe))
    val lifted = toolbox.select("Ascribe").appliedTo(expr, tpe)
    map(lifted, raw)
    lifted
  }

  def liftAnnotated(expr: TermTree, annots: Seq[TermTree]): TermTree = {
    val raw = Annotated(unliftAs[TermTree](expr), annots.map(unlift))
    val liftedAnnots = liftSeq(annots)
    val lifted = toolbox.select("Annotated").appliedTo(expr, liftedAnnots)
    map(lifted, raw)
    lifted
  }

  def liftFunction(params: Seq[TermTree], body: TermTree): TermTree = {
    val raw = Function(params.map(unliftAs[Param]), unliftAs[TermTree](body))
    val liftedParams = liftSeq(params)
    val lifted = toolbox.select("Function").appliedTo(liftedParams, body)
    map(lifted, raw)
    lifted
  }

  def liftNew(tpe: TermTree): TermTree = {
    val raw = New(unlift(tpe))
    val lifted = toolbox.select("New").appliedTo(tpe)
    map(lifted, raw)
    lifted
  }

  def liftNamed(name: String, expr: TermTree): TermTree = {
    val raw = Named(name, unliftAs[TermTree](expr))
    val lifted = toolbox.select("Named").appliedTo(Lit(name), expr)
    map(lifted, raw)
    lifted
  }

  def liftFor(enums: Seq[TermTree], body: TermTree): TermTree = {
    val raw = For(enums.map(unlift), unliftAs[TermTree](body))
    val lifted = toolbox.select("For").appliedTo(liftSeq(enums), body)
    map(lifted, raw)
    lifted
  }

  def liftForYield(enums: Seq[TermTree], body: TermTree): TermTree = {
    val raw = ForYield(enums.map(unlift), unliftAs[TermTree](body))
    val lifted = toolbox.select("ForYield").appliedTo(liftSeq(enums), body)
    map(lifted, raw)
    lifted
  }

  def liftGenFrom(pat: TermTree, rhs: TermTree): TermTree = {
    val raw = GenFrom(unliftAs[TermTree](pat), unliftAs[TermTree](rhs))
    val lifted = toolbox.select("GenFrom").appliedTo(pat, rhs)
    map(lifted, raw)
    lifted
  }

  def liftGenAlias(pat: TermTree, rhs: TermTree): TermTree = {
    val raw = GenAlias(unliftAs[TermTree](pat), unliftAs[TermTree](rhs))
    val lifted = toolbox.select("GenAlias").appliedTo(pat, rhs)
    map(lifted, raw)
    lifted
  }

  def liftGuard(cond: TermTree): TermTree = {
    val raw = Guard(unliftAs[TermTree](cond))
    val lifted = toolbox.select("Guard").appliedTo(cond)
    map(lifted, raw)
    lifted
  }

  def liftBind(name: String, expr: TermTree): TermTree = {
    val raw = Bind(name, unliftAs[TermTree](expr))
    val lifted = toolbox.select("Bind").appliedTo(expr)
    map(lifted, raw)
    lifted
  }

  def liftAlternative(trees: Seq[TermTree]): TermTree = {
    val raw = Alternative(trees.map(unliftAs[TermTree]))
    val lifted = toolbox.select("Alternative").appliedTo(liftSeq(trees))
    map(lifted, raw)
    lifted
  }

  /* -------------------- importing  ----------------------------- */
  def liftImport(items: Seq[TermTree]): TermTree = {
    val raw = Import(items.map(unlift))
    val lifted = toolbox.select("Import").appliedTo(liftSeq(items))
    map(lifted, raw)
    lifted
  }

  def liftImportItem(ref: TermTree, importees: Seq[TermTree]): TermTree = {
    val raw = ImportItem(unlift(ref), importees.map(unlift))
    val lifted = toolbox.select("ImportItem").appliedTo(liftSeq(importees))
    map(lifted, raw)
    lifted
  }

  def liftImportName(name: String): TermTree = {
    val raw = ImportName(name)
    val lifted = toolbox.select("ImportName").appliedTo(Lit(name))
    map(lifted, raw)
    lifted
  }

  def liftImportRename(from: String, to: String): TermTree = {
    val raw = ImportRename(from, to)
    val lifted = toolbox.select("ImportRename").appliedTo(Lit(from), Lit(to))
    map(lifted, raw)
    lifted
  }

  def liftImportHide(name: String): TermTree = {
    val raw = ImportHide(name)
    val lifted = toolbox.select("ImportHide").appliedTo(Lit(name))
    map(lifted, raw)
    lifted
  }

  /* -------------------- lifting types -------------------------- */
  def liftTypeIdent(name: String): TermTree = {
    val raw = TypeIdent(name)
    val lifted = toolbox.select("TypeIdent").appliedTo(Lit(name))
    map(lifted, raw)
    lifted
  }

  def liftTypeSelect(qual: TermTree, name: String): TermTree = {
    val raw = TypeSelect(unlift(qual), name)
    val lifted = toolbox.select("TypeSelect").appliedTo(qual, Lit(name))
    map(lifted, raw)
    lifted
  }

  def liftTypeApplyInfix(lhs: TermTree, op: String, rhs: TermTree): TermTree = {
    val raw = TypeApplyInfix(unliftAs[TypeTree](lhs), op, unliftAs[TypeTree](rhs))
    val lifted = toolbox.select("TypeApplyInfix").appliedTo(lhs, liftLit(op), rhs)
    map(lifted, raw)
    lifted
  }

  def liftTypeFunction(params: Seq[TermTree], res: TermTree): TermTree = {
    val raw = TypeFunction(params.map(unliftAs[TypeTree]), unliftAs[TypeTree](res))
    val liftedParams = liftSeq(params)
    val lifted = toolbox.select("TypeFunction").appliedTo(liftedParams, res)
    map(lifted, raw)
    lifted
  }

  def liftTypeRefine(tpe: TermTree, stats: Seq[TermTree]) = {
    val raw = TypeRefine(if (tpe == null) None else Some(unliftAs[TypeTree](tpe)), stats.map(unlift))
    val liftedTpe = liftOption(tpe)
    val liftedStats = liftSeq(stats)
    val lifted = toolbox.select("TypeRefine").appliedTo(liftedTpe, liftedStats)
    map(lifted, raw)
    lifted
  }

  def liftTypeAnd(lhs: TermTree, rhs: TermTree): TermTree = {
    val raw = TypeAnd(unliftAs[TypeTree](lhs), unliftAs[TypeTree](rhs))
    val lifted = toolbox.select("TypeAnd").appliedTo(lhs, rhs)
    map(lifted, raw)
    lifted
  }

  def liftTypeOr(lhs: TermTree, rhs: TermTree): TermTree = {
    val raw = TypeOr(unliftAs[TypeTree](lhs), unliftAs[TypeTree](rhs))
    val lifted = toolbox.select("TypeOr").appliedTo(lhs, rhs)
    map(lifted, raw)
    lifted
  }

  def liftTypeAnnotated(tpe: TermTree, annots: Seq[TermTree]): TermTree = {
    val raw = TypeAnnotated(unliftAs[TypeTree](tpe), annots.map(unlift))
    val liftedAnnots = liftSeq(annots)
    val lifted = toolbox.select("TypeAnnotated").appliedTo(tpe, liftedAnnots)
    map(lifted, raw)
    lifted
  }

  def liftTypeSingleton(ref: TermTree): TermTree = {
    val raw = TypeSingleton(unlift(ref))
    val lifted = toolbox.select("TypeSingleton").appliedTo(ref)
    map(lifted, raw)
    lifted
  }

  def liftTypeApply(tpe: TermTree, args: Seq[TermTree]): TermTree = {
    val raw = TypeApply(unliftAs[TypeTree](tpe), args.map(unliftAs[TypeTree]))
    val liftedArgs = liftSeq(args)
    val lifted = toolbox.select("TypeApply").appliedTo(tpe, liftedArgs)
    map(lifted, raw)
    lifted
  }

  def liftTypeByName(tpe: TermTree): TermTree = {
    val raw = TypeByName(unliftAs[TypeTree](tpe))
    val lifted = toolbox.select("TypeByName").appliedTo(tpe)
    map(lifted, raw)
    lifted
  }

  def liftTypeRepeated(tpe: TermTree): TermTree = {
    val raw = TypeRepeated(unliftAs[TypeTree](tpe))
    val lifted = toolbox.select("TypeRepeated").appliedTo(tpe)
    map(lifted, raw)
    lifted
  }

  def liftTypeBounds(lo: TermTree, hi: TermTree): TermTree = {
    val raw = TypeBounds(
      if (lo == null) None else Some(unliftAs[TypeTree](lo)),
      if (hi == null) None else Some(unliftAs[TypeTree](hi))
    )
    val liftedLO = liftOption(lo)
    val liftedHI = liftOption(hi)
    val lifted = toolbox.select("TypeBounds").appliedTo(liftedLO, liftedHI)
    map(lifted, raw)
    lifted
  }
}

