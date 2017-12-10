package scala.gestalt.quasiquotes

import scala.collection.immutable.Seq
import scala.{meta => m}
import scala.compat.Platform.EOL
import scala.gestalt._
import scala.gestalt.options.unsafe

/** Lift scala.meta trees as trees */
class QuoteTpd(args: List[tpd.Tree], enclosingPos: Position) {
  type Quasi = m.internal.ast.Quasi

  // lifted trees
  lazy val scalaNil       = untpd.root.select("scala.Nil")
  lazy val scalaList      = untpd.root.select("scala.List")
  lazy val scalaSome      = untpd.root.select("scala.Some")
  lazy val scalaNone      = untpd.root.select("scala.None")

  private def tpd(path: String): untpd.TermTree = Path("scala.gestalt.tpd." + path)

  private def Path(path: String): untpd.TermTree = {
    val parts = path.split('.')

    parts.tail.foldLeft[untpd.TermTree](untpd.Ident.apply(parts.head)) { (prefix, name) =>
      prefix.select(name)
    }
  }

  /**
    * (trees: Seq[m.Tree[A]]) => untpd.TermTree[Seq[tpd.Tree[A]]]
    **/
  def liftSeq(trees: Seq[m.Tree]): untpd.TermTree =  {
    def loop(trees: List[m.Tree], acc: Option[untpd.TermTree], prefix: List[m.Tree]): untpd.TermTree = trees match {
      case (quasi: Quasi) +: rest if quasi.rank == 1 =>
        if (acc.isEmpty) {
          if (prefix.isEmpty) loop(rest, Some(liftQuasi(quasi)), Nil)
          else loop(rest, Some(prefix.foldRight(liftQuasi(quasi))((curr, acc) => {
            val currElement = lift(curr)
            untpd.Infix(currElement, "+:", acc)
          })), Nil)
        } else {
          require(prefix.isEmpty)
          loop(rest, Some(untpd.Infix(acc.get, "++", liftQuasi(quasi))), Nil)
        }
      case other +: rest =>
        if (acc.isEmpty) loop(rest, acc, prefix :+ other)
        else {
          require(prefix.isEmpty)
          loop(rest, Some(untpd.Infix(acc.get, ":+", lift(other))), Nil)
        }
      case Nil =>
        if (acc.isEmpty)
          scalaList.appliedTo(prefix.map(lift): _*)
        else acc.get
    }

    loop (trees.toList, None, Nil)
  }

  /** {{{
    * (treess: Seq[Seq[m.Tree[?]]]) => untpd.TermTree[Seq[Seq[tpd.Tree[?]]]]
    * }}}*/
  def liftSeqSeq(treess: Seq[Seq[m.Tree]]): untpd.TermTree = {
    val tripleDotQuasis = treess.flatten.collect{ case quasi: Quasi if quasi.rank == 2 => quasi }
    if (tripleDotQuasis.length == 0) {
      val args = treess.map(liftSeq)
      scalaList.appliedTo(args: _*)
    } else if (tripleDotQuasis.length == 1) {
      if (treess.flatten.length == 1) liftQuasi(tripleDotQuasis(0))
      else {
        error("implementation restriction: can't mix ...$ with anything else in parameter lists." +
          EOL + "See https://github.com/scalameta/scalameta/issues/406 for details.", enclosingPos)
        untpd.Lit(null)
      }
    } else {
      error(m.internal.parsers.Messages.QuasiquoteAdjacentEllipsesInPattern(2), enclosingPos)
      untpd.Lit(null)
    }
  }

  /** (tree: Quasi, Int, Boolean) => untpd.TermTree[tpd.Tree[?]] */
  def liftQuasi(quasi: Quasi): untpd.TermTree = {
    if (quasi.rank > 0) return liftQuasi(quasi.tree.asInstanceOf[Quasi])

    quasi.tree match {
      case m.Term.Name(Hole(i)) => args(i)
      case m.Type.Name(Hole(i)) => args(i)
    }
  }

  private object Qualifier {
    def unapply(tree: m.Tree) = tree match {
      case m.Ctor.Ref.Select(qual, inner) => Some(inner -> Some(qual))
      case inner => Some(inner -> None)
    }
  }

  /**
    * (tree: m.Tree[A]) => untpd.TermTree[tpd.Tree[A]]
    */
  def lift(tree: m.Tree): untpd.TermTree = tree match {
    case quasi: Quasi  =>
      liftQuasi(quasi)

    case m.Lit(value) =>
      tpd("Lit").appliedTo(untpd.Lit(value))

    // case m.Name.Anonymous() =>
    // case m.Name.Indeterminate(name) =>

    case m.Term.Select(qual, quasi: Quasi) =>
      tpd("Select").appliedTo(lift(qual), liftQuasi(quasi))
    case m.Term.Select(qual, m.Term.Name(name)) =>
      tpd("Select").appliedTo(lift(qual), untpd.Lit(name))
    case m.Term.Apply(fun, args) =>
      // magic happens here with ...$args
      args match {
        case Seq(quasi: Quasi) if quasi.rank == 2 =>
          tpd("ApplySeq").appliedTo(lift(fun), liftQuasi(quasi))
        case _ =>
          tpd("Apply").appliedTo(lift(fun), liftSeq(args))
      }
    case m.Term.ApplyInfix(lhs, m.Term.Name(name), Nil, arg :: Nil)  =>
      tpd("Apply").appliedTo(tpd("Select").appliedTo(lift(lhs), untpd.Lit(name)), scalaList.appliedTo(lift(arg)))
    case m.Term.ApplyType(fun, targs) =>
      tpd("ApplyType").appliedTo(lift(fun), liftSeq(targs))
    case m.Term.ApplyUnary(m.Term.Name(op), arg) =>
      tpd("Prefix").appliedTo(untpd.Lit(op), lift(arg))
    case m.Term.Assign(lhs, rhs) =>
      tpd("Assign").appliedTo(lift(lhs), lift(rhs))
    // case m.Term.Update(fun, argss, rhs) =>
    //   require(argss.size > 0)
    //   tpd("Update").appliedTo(lift(fun), liftSeqSeq(argss), lift(rhs))
    // case m.Term.Return(expr) =>
    //   tpd("Return").appliedTo(lift(expr))
    // case m.Term.Throw(expr) =>
    //   tpd("Throw").appliedTo(lift(expr))
    case m.Term.Ascribe(expr, tpe) =>
      tpd("Ascribe").appliedTo(lift(expr), lift(tpe))
    case m.Term.Tuple(args) =>
      tpd("Tuple").appliedTo(liftSeq(args))
    case m.Term.Block(stats) =>
      if (stats.size == 0)
        tpd("Block").appliedTo(scalaNil, tpd("Lit").appliedTo(untpd.Lit(())))
      else if (stats.size == 1)
        tpd("Block").appliedTo(scalaNil, lift(stats.head))
      else
        tpd("Block").appliedTo(liftSeq(stats.init), lift(stats.last))
    case m.Term.If(cond, thenp, elsep) =>
      tpd("If").appliedTo(lift(cond), lift(thenp), lift(elsep))
    case m.Term.While(expr, body) =>
      tpd("While").appliedTo(lift(expr), lift(body))
    case m.Term.Do(body, expr) =>
      tpd("DoWhile").appliedTo(lift(body), lift(expr))
    // case m.Term.New(m.Template(Nil, Seq(mctor), m.Term.Param(Nil, m.Name.Anonymous(), None, None), None)) =>
    //   liftNewInstance(mctor)
    // case m.Term.Arg.Repeated(expr) =>
    //   tpd("Repeated").appliedTo(lift(expr))

    case _ =>
      error("Not supported expression in typed quasiquote: " + tree.show[m.Syntax], enclosingPos)
      tpd("Lit").appliedTo(untpd.Lit(null))
  }
}
