package scala.gestalt

import scala.{meta => m}
import m.dialects.Dotty
import scala.gestalt.api._

package object quasiquotes {
  // term dialect suffices the purpose, no need for pattern dialect
  private val quasiquoteTermDialect = m.dialects.Dotty.copy(allowTermUnquotes = true, allowMultilinePrograms = true)

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

  private def instantiateParser(parserName: String): m.Input => m.Tree = {
    val parsersModuleClass = java.lang.Class.forName("scala.meta.quasiquotes.package$", true, this.getClass.getClassLoader)
    val parsersModule = parsersModuleClass.getField("MODULE$").get(null)
    val parserModuleGetter = parsersModule.getClass.getDeclaredMethod(parserName)
    val parserModuleInstance = parserModuleGetter.invoke(parsersModule)
    val parserMethod = parserModuleInstance.getClass.getDeclaredMethods().find(_.getName == "parse").head
    (input: m.Input) => {
      try parserMethod.invoke(parserModuleInstance, input, quasiquoteTermDialect).asInstanceOf[m.Tree]
      catch { case ex: java.lang.reflect.InvocationTargetException => throw ex.getTargetException }
    }
  }

  /** Resugar tree into string interpolation */
  private def resugar(parts: List[String]): String = {
    parts.init.zipWithIndex.map { case (part, i) =>
      s"$part${Hole(i)}"
    }.mkString("", "", parts.last)
  }

  implicit class Helper(val sc: StringContext) {
    // statements and terms
    def q[T <: untpd.Tree](any: Any*): T = meta {
      System.out.println("expanding macros.....")

      val tpd.SeqLiteral(args) = any
      val tpd.Apply(_, tpd.Apply(_, tpd.SeqLiteral(parts) :: Nil) :: Nil) = prefix

      println("parts: " + parts.map(_.show))
      println("args: " + args.map(_.show))

      val code = resugar(parts.map { case tpd.Lit(str : String) => str })
      val parser = instantiateParser(parserMap("q"))
      val mTree = parser(m.Input.String(code))
      val quote = new QuoteUntpd(args, prefix.pos)

      quote.lift(mTree)
    }

    // type trees
    def t(any: Any*): untpd.TypeTree = meta {
      val tpd.SeqLiteral(args) = any
      val tpd.Apply(_, tpd.Apply(_, tpd.SeqLiteral(parts) :: Nil) :: Nil) = prefix

      val code = resugar(parts.map { case tpd.Lit(str: String) => str })
      val parser = instantiateParser(parserMap("t"))
      val mTree = parser(m.Input.String(code))
      val quote = new QuoteUntpd(args, prefix.pos)

      quote.lift(mTree)
    }

    def tq[T](any: Any*): T = meta {
      val tpd.SeqLiteral(args) = any
      val tpd.Apply(_, tpd.Apply(_, tpd.SeqLiteral(parts) :: Nil) :: Nil) = prefix

      val code = resugar(parts.map { case tpd.Lit(str: String) => str })
      val parser = instantiateParser(parserMap("q"))
      val mTree = parser(m.Input.String(code))
      val quote = new QuoteTpd(args, prefix.pos)

      quote.lift(mTree)
    }
  }
}
