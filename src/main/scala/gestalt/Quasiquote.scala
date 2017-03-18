package scala.gestalt

import scala.{meta => m}
import m.dialects.Dotty

object Quasiquote {
  type MetaParser = (m.Input, m.Dialect) => m.Tree
  type QuoteLabel = String

  // term dialect suffices the purpose, no need for pattern dialect
  val quasiquoteTermDialect = m.Dialect.forName("QuasiquoteTerm(Dotty, Multi)")

  private val StringContextName = "StringContext"
  private val ApplyName = "apply"
  private val UnApplyName = "unapply"

  private val parserMap = Map(
    "q"          ->    "XtensionQuasiquoteTerm",
    "arg"        ->    "XtensionQuasiquoteTermArg",
    "param"      ->    "XtensionQuasiquoteTermParam",
    "t"          ->    "XtensionQuasiquoteType",
    "targ"       ->    "XtensionQuasiquoteTypeArg",
    "tparam"     ->    "XtensionQuasiquoteTypeParam",
    "p"          ->    "XtensionQuasiquoteCaseOrPattern",
    "parg"       ->    "XtensionQuasiquotePatternArg",
    "pt"         ->    "XtensionQuasiquotePatternType",
    "ctor"       ->    "XtensionQuasiquoteCtor",
    "template"   ->    "XtensionQuasiquoteTemplate",
    "mod"        ->    "XtensionQuasiquoteMod",
    "enumerator" ->    "XtensionQuasiquoteEnumerator",
    "importer"   ->    "XtensionQuasiquoteImporter",
    "importee"   ->    "XtensionQuasiquoteImportee",
    "source"     ->    "XtensionQuasiquoteSource"
  )

  private[gestalt] object Hole {
    val pat = java.util.regex.Pattern.compile("^placeholder(\\d+)$")
    def apply(i: Int) = s"$$placeholder$i"
    def unapply(s: String): Option[Int] = {
      val m = pat.matcher(s)
      if (m.find()) Some(m.group(1).toInt) else None
    }
  }

  private def instantiateParser(parserName: String): MetaParser = {
    val parsersModuleClass = Class.forName("scala.meta.quasiquotes.package$", true, this.getClass.getClassLoader)
    val parsersModule = parsersModuleClass.getField("MODULE$").get(null)
    val parserModuleGetter = parsersModule.getClass.getDeclaredMethod(parserName)
    val parserModuleInstance = parserModuleGetter.invoke(parsersModule)
    val parserMethod = parserModuleInstance.getClass.getDeclaredMethods().find(_.getName == "parse").head
    (input: m.Input, dialect: m.Dialect) => {
      try parserMethod.invoke(parserModuleInstance, input, dialect).asInstanceOf[m.Tree]
      catch { case ex: java.lang.reflect.InvocationTargetException => throw ex.getTargetException }
    }
  }
}

/** Implementation of quasiquotes
 *
 * @param t           the toolbox to use
 * @param toolboxName the name of the toolbox in the local environment
 */
class Quasiquote(val t: Toolbox, val toolboxName: String) {
  import t._
  import Quasiquote._

  def expand(label: String, tree: Tree, parts: List[String], unquotes: List[Tree], isPattern: Boolean): Tree = {
    val code = resugar(parts)
    val parser = instantiateParser(parserMap(label))
    val mTree = parser(m.Input.String(code), quasiquoteTermDialect)
    val quote = new Quote(t, toolboxName) {
      val args = unquotes.asInstanceOf[List[this.t.Tree]]    // fix compiler stupidity
      val isTerm = !isPattern
      val enclosingTree = tree.asInstanceOf[this.t.Tree]
    }

    // compiler stupidity
    quote.lift(mTree).asInstanceOf[Tree]
  }

  /** Resugar tree into string interpolation */
  private def resugar(parts: List[String]): String = {
    parts.init.zipWithIndex.map { case (part, i) =>
      s"$part${Hole(i)}"
    }.mkString("", "", parts.last)
  }
}
