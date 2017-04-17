import scala.gestalt._
import parsing._

object Parsing {
  def parseType(tb: Toolbox, code: String): tb.Tree = {
    val parser = new Parsers.Parser(tb, "toolbox", true, code.toCharArray) {
      val splices = Nil
    }
    parser.typ().asInstanceOf[tb.Tree]
  }

  def helper(tb1: Toolbox): TreeHelper = {
    new TreeHelper {
      val tb = tb1
      val tbName = "toolbox"
    }
  }

  def testTypes(): Boolean = meta {
    val helper = Parsing.helper(toolbox)
    def parse(code: String) = Parsing.parseType(toolbox, code)

    var actual = parse("Int")
    var expect = helper.liftTypeIdent("Int")
    assert(actual.toString == expect.toString)

    actual = parse("scala.Nil.type")
    expect = helper.liftTypeSingleton(helper.liftSelect(helper.liftIdent("scala"), "Nil"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a.T")
    expect = helper.liftTypeSelect(helper.liftIdent("a"), "T")
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a.b.T")
    expect = helper.liftTypeSelect(helper.liftSelect(helper.liftIdent("a"), "b"), "T")
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("Int & String")
    expect = helper.liftTypeAnd(helper.liftTypeIdent("Int"), helper.liftTypeIdent("String"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("Int | String")
    expect = helper.liftTypeOr(helper.liftTypeIdent("Int"), helper.liftTypeIdent("String"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("List[Int]")
    expect = helper.liftTypeApply(helper.liftTypeIdent("List"), List(helper.liftTypeIdent("Int")))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("Int => String")
    expect = helper.liftTypeFunction(List(helper.liftTypeIdent("Int")), helper.liftTypeIdent("String"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("(Int, Int) => String")
    expect = helper.liftTypeFunction(List(helper.liftTypeIdent("Int"), helper.liftTypeIdent("Int")), helper.liftTypeIdent("String"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("Int ~ String")
    expect = helper.liftTypeApplyInfix(helper.liftTypeIdent("Int"), "~", helper.liftTypeIdent("String"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    toolbox.Lit(true)
  }
}
