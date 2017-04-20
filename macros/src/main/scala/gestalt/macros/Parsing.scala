import scala.annotation.StaticAnnotation
import scala.gestalt._
import parsing._

object Parsing {
  def parseType(tb: StructToolbox, code: String): tb.Tree = {
    val parser = new Parsers.Parser(tb, "toolbox", true, code.toCharArray) {
      val splices = Nil
    }
    parser.unlift(parser.typ()).asInstanceOf[tb.Tree]
  }

  def helper(tb1: StructToolbox): TreeHelper = {
    new TreeHelper {
      val tb = tb1
      val tbName = "toolbox"
    }
  }
}

class testTypes extends StaticAnnotation {
  def apply(defn: Any): Any = meta {
    import toolbox._
    val helper = Parsing.helper(toolbox)

    def parse(code: String) = Parsing.parseType(toolbox, code)

    var actual: AnyRef = parse("Int")
    var expect: AnyRef = Ident("Int")
    assert(actual.toString == expect.toString)

    actual = parse("scala.Nil.type")
    expect = TypeSingleton(Select(Ident("scala"), "Nil"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a.T")
    expect = TypeSelect(Ident("a"), "T")
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a.b.T")
    expect = TypeSelect(Select(Ident("a"), "b"), "T")
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("Int & String")
    expect = TypeAnd(TypeIdent("Int"), TypeIdent("String"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("Int | String")
    expect = TypeOr(TypeIdent("Int"), TypeIdent("String"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("List[Int]")
    expect = TypeApply(TypeIdent("List"), List(TypeIdent("Int")))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("Int => String")
    expect = TypeFunction(List(TypeIdent("Int")), TypeIdent("String"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("(Int, Int) => String")
    expect = TypeFunction(List(TypeIdent("Int"), TypeIdent("Int")), TypeIdent("String"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("Int ~ String")
    expect = TypeApplyInfix(TypeIdent("Int"), "~", TypeIdent("String"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    defn
  }
}
