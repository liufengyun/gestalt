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

  def parseTerm(tb: StructToolbox, code: String): tb.Tree = {
    val parser = new Parsers.Parser(tb, "toolbox", true, code.toCharArray) {
      val splices = Nil
    }
    parser.unlift(parser.block()).asInstanceOf[tb.Tree]
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

class testTerms extends StaticAnnotation {
  def apply(defn: Any): Any = meta {
    import toolbox._
    val helper = Parsing.helper(toolbox)

    def parse(code: String) = Parsing.parseTerm(toolbox, code)

    var actual: AnyRef = parse("3")
    var expect: AnyRef = Lit(3)
    assert(actual.toString == expect.toString)

    actual = parse("a")
    expect = Ident("a")
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a.b")
    expect = Select(Ident("a"), "b")
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a.b.c")
    expect = Select(Select(Ident("a"), "b"), "c")
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("this")
    expect = This("")
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("A.this")
    expect = This("A")
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("super.x")
    expect = Select(Super("", ""), "x")
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("super[A].f(x)")
    expect = Apply(Select(Super("", "A"), "f"), List(Ident("x")))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("A.super.x")
    expect = Select(Super("A", ""), "x")
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("C.super[A].x")
    expect = Select(Super("C", "A"), "x")
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("f(a, -4)")
    expect = Apply(Ident("f"), List(Ident("a"), Lit(-4)))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a = b")
    expect = Assign(Ident("a"), Ident("b"))
    //println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("o.a = b")
    expect = Assign(Select(Ident("o"), "a"), Ident("b"))
    //println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a = b + c")
    expect = Assign(Ident("a"), Infix(Ident("b"), "+", Ident("c")))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("if (a) b else c")
    expect = If(Ident("a"), Ident("b"), Some(Ident("c")))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a: A")
    expect = Ascribe(Ident("a"), TypeIdent("A"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("!a")
    expect = Prefix("!", Ident("a"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a+")
    expect = Postfix(Ident("a"), "+")
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a + b")
    expect = Infix(Ident("a"), "+", Ident("b"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)


    defn
  }
}

