package scala.gestalt
package parsing

import java.util.IdentityHashMap

trait TreeHelper {
  val tb: Toolbox
  val tbName: String

  import tb._

  private implicit class TreeOps(val tree: Tree) {
    def select(name: String): Tree = Select(tree, name)

    def appliedTo(args: Tree*): Tree = Apply(tree, args.toList)

    def appliedToType(args: TypeTree*): Tree = ApplyType(tree, args.toList)
  }

  // sometimes, the parser needs to check the unlifted Tree
  val liftedToUnlifted = new IdentityHashMap[Tree, Tree]
  def unlift(tree: Tree): Tree = liftedToUnlifted.get(tree)
  def unliftTypeTree(tree: Tree): TypeTree = liftedToUnlifted.get(tree).asInstanceOf[TypeTree]
  def map(lifted: Tree, unlifted: Tree): Unit = liftedToUnlifted.put(lifted, unlifted)

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

  def liftBlock(trees: Seq[Tree]): Tree = {
    val raw = Block(trees.map(unlift))
    val args = trees.foldLeft(scalaNil) { (acc, tree) => tb.Infix(tree, "::", acc) }
    val lifted = toolbox.select("Block").appliedTo(args)
    map(lifted, raw)
    lifted
  }

  def liftTuple(trees: Seq[Tree]): Tree = {
    val raw = Tuple(trees.map(unlift))
    val args = trees.foldLeft(scalaNil) { (acc, tree) => tb.Infix(tree, "::", acc) }
    val lifted = toolbox.select("Tuple").appliedTo(args)
    map(lifted, raw)
    lifted
  }

  def liftInterpolate(tag: String, parts: Seq[String], args: Seq[Tree]): Tree = {
    val raw = Interpolate(tag, parts, args.map(unlift))
    val liftedParts = parts.foldLeft(scalaNil) { (acc, str) => tb.Infix(Lit(str), "::", acc) }
    val liftedArgs = args.foldLeft(scalaNil) { (acc, tree) => tb.Infix(tree, "::", acc) }
    val lifted = toolbox.select("Interpolate").appliedTo(Lit(tag), liftedParts, liftedArgs)
    map(lifted, raw)
    lifted
  }

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

