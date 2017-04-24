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

  def parseDefinition(tb: StructToolbox, code: String, debug: Boolean = false): tb.Tree = {
    val parser = new Parsers.Parser(tb, "toolbox", true, code.toCharArray) {
      val splices = Nil
    }
    val lifted = parser.templateStat()
    if (debug) println("lifted: " + lifted)
    parser.unlift(lifted).asInstanceOf[tb.Tree]
  }
}

class testTypes extends StaticAnnotation {
  def apply(defn: Any): Any = meta {
    import toolbox._

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

    actual = parse("(Int, String)")
    expect = TypeTuple(TypeIdent("Int") :: TypeIdent("String") :: Nil)
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    // TODO: TypeRefine

    // actual = parse(" <: A")
    // expect = TypeBounds(None, Some(TypeIdent("A")))
    // println(s"expect: $expect"); println(s"actual: $actual")
    // assert(actual.toString == expect.toString)

    // actual = parse(" >: A")
    // expect = TypeBounds(Some(TypeIdent("A")), None)
    // println(s"expect: $expect"); println(s"actual: $actual")
    // assert(actual.toString == expect.toString)

    // actual = parse(" >: C <: A")
    // expect = TypeBounds(Some(TypeIdent("C")), Some(TypeIdent("A")))
    // println(s"expect: $expect"); println(s"actual: $actual")
    // assert(actual.toString == expect.toString)

    actual = parse("(=> Int) => Int")
    expect = TypeFunction(List(TypeByName(TypeIdent("Int"))), TypeIdent("Int"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    // actual = parse("A*")
    // expect = TypeRepeated(TypeIdent("A"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    // assert(actual.toString == expect.toString)

    actual = parse("(T @alert(1)) => Int")
    expect = TypeFunction(
      TypeAnnotated(TypeIdent("T"), List(InitCall(None, "alert", Nil, List(List(Lit(1)))))) :: Nil,
      TypeIdent("Int")
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("(T @check @unsafe) => Int")
    expect = TypeFunction(
      TypeAnnotated(TypeIdent("T"), List(InitCall(None, "check", Nil, Nil), InitCall(None, "unsafe", Nil, Nil))) :: Nil,
      TypeIdent("Int")
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)


    defn
  }
}

class testTerms extends StaticAnnotation {
  def apply(defn: Any): Any = meta {
    import toolbox._

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

    actual = parse("f(a = 3)")
    expect = Apply(Ident("f"), List(Named("a", Lit(3))))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("f(a: _*)")
    expect = Apply(Ident("f"), List(Repeated(Ident("a"))))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("f[A](-3.12)")
    expect = Apply(ApplyType(Ident("f"), List(TypeIdent("A"))), List(Lit(-3.12)))
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

    actual = parse("return")
    expect = Return
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("return null")
    expect = Return(Lit(null))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("throw a")
    expect = Throw(Ident("a"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a: Int")
    expect = Ascribe(Ident("a"), TypeIdent("Int"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a: @check")
    expect = Annotated(Ident("a"), List(InitCall(None, "check", Nil, Nil)))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a: @alert(1)")
    expect = Annotated(Ident("a"), List(InitCall(None, "alert", Nil, List(List(Lit(1))))))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a: @check @unsafe")
    expect = Annotated(Ident("a"), List(InitCall(None, "check", Nil, Nil), InitCall(None, "unsafe", Nil, Nil)))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("(a, b)")
    expect = Tuple(Ident("a") :: Ident("b") :: Nil)
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("{ a ; b }")
    expect = Block(Ident("a") :: Ident("b") :: Nil)
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a match { case x => x }")
    expect = Match(Ident("a"), Case(Ident("x"), None, Ident("x")) :: Nil)
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a match { case Some(x) => x }")
    expect = Match(Ident("a"), Case(Apply(Ident("Some"), Ident("x") :: Nil), None, Ident("x")) :: Nil)
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a match { case Some(_) | None => 3 }")
    expect = Match(Ident("a"),
      Case(
        Alternative(Apply(Ident("Some"), Ident("_") :: Nil) :: Ident("None") :: Nil),
        None,
        Lit(3)
      ) :: Nil
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("a match { case x @ Some(_) => 3 }")
    expect = Match(Ident("a"),
      Case(
        Bind("x", Apply(Ident("Some"), Ident("_") :: Nil)),
        None,
        Lit(3)
      ) :: Nil
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("try { f(a) } catch { case _ => }")
    expect = Try(Apply(Ident("f"), Ident("a") :: Nil),
      Case(Ident("_"), None, Block(Nil)) :: Nil,
      None
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("try { f(a) } catch { case _ => } finally { a = 3 }")
    expect = Try(Apply(Ident("f"), Ident("a") :: Nil),
      Case(Ident("_"), None, Block(Nil)) :: Nil,
      Some(Assign(Ident("a"), Lit(3)))
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("try { f(a) } catch handler finally { a = 3 }")
    expect = Try(Apply(Ident("f"), Ident("a") :: Nil),
      Ident("handler"),
      Some(Assign(Ident("a"), Lit(3)))
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("x => x * x")
    expect = Function(
      Param(emptyMods, "x", None, None) :: Nil,
      Infix(Ident("x"), "*", Ident("x"))
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("{ case Some(x) => x ; case None => 0 }")
    expect = PartialFunction(
      Case(Apply(Ident("Some"), Ident("x") :: Nil), None, Ident("x")) ::
      Case(Ident("None"), None, Lit(0)) :: Nil
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("while (x > 3) x -= 1")
    expect = While(
      Infix(Ident("x"), ">", Lit(3)),
      Infix(Ident("x"), "-=", Lit(1))
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("do { x -= 1 } while (x > 3)")
    expect = DoWhile(
      Infix(Ident("x"), "-=", Lit(1)),
      Infix(Ident("x"), ">", Lit(3))
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("new A(1)")
    expect = New(InitCall(None, "A", Nil, List(List(Lit(1)))))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("for (x <- xs ; if x > 0) yield x * x")
    expect = ForYield(
      GenFrom(Ident("x"), Ident("xs")) :: Guard(Infix(Ident("x"), ">", Lit(0))):: Nil,
      Infix(Ident("x"), "*", Ident("x"))
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("for (x <- xs ; if x > 0; Some(y) = x) println(y)")
    expect = For(
      GenFrom(Ident("x"), Ident("xs")) ::
        Guard(Infix(Ident("x"), ">", Lit(0)))::
        GenAlias(Apply(Ident("Some"), Ident("y") :: Nil), Ident("x")):: Nil,
      Apply(Ident("println"), Ident("y") :: Nil)
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("import a.b.c")
    expect = Import(ImportItem(Select(Ident("a"), "b"), ImportName("c") :: Nil) :: Nil)
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("import a.b.c, y.z._")
    expect = Import(
      ImportItem(Select(Ident("a"), "b"), ImportName("c") :: Nil) ::
      ImportItem(Select(Ident("y"), "z"), ImportName("_") :: Nil) :: Nil
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("import a.b.{ x => y, u => _, _}")
    expect = Import(
      ImportItem(
        Select(Ident("a"), "b"),
        ImportRename("x", "y") ::
          ImportHide("u") ::
          ImportName("_") :: Nil
      ) :: Nil
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    defn
  }
}

class testDefinition extends StaticAnnotation {
  def apply(defn: Any): Any = meta {
    import toolbox._

    def parse(code: String, debug: Boolean = false) = Parsing.parseDefinition(toolbox, code, debug)

    var actual: AnyRef = parse("val x = 3")
    var expect: AnyRef = ValDef(emptyMods, "x", None, Lit(3))
    assert(actual.toString == expect.toString)
    var mods = actual.asInstanceOf[ValDef].mods
    assert(!mods.isPrivate)
    assert(!mods.isLazy)

    actual = parse("implicit val x = 3").asInstanceOf[ValDef]
    expect = ValDef(emptyMods, "x", None, Lit(3))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)
    mods = actual.asInstanceOf[ValDef].mods
    assert(mods.isImplicit)

    actual = parse("val x, y, z: Int")
    expect = SeqDecl(emptyMods, List("x", "y", "z"), TypeIdent("Int"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("val Some(x): M = y")
    expect = PatDef(emptyMods, Apply(Ident("Some"), Ident("x") :: Nil), Some(TypeIdent("M")), Ident("y"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("private[core] val x: Int").asInstanceOf[ValDecl]
    expect = ValDecl(emptyMods, "x", TypeIdent("Int"))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)
    mods = actual.asInstanceOf[ValDecl].mods
    assert(mods.isPrivate)
    assert(mods.privateWithin == "core")

    actual = parse("protected[core] def f: Int = 3").asInstanceOf[DefDef]
    expect = DefDef(emptyMods, "f", Nil, Nil, Some(TypeIdent("Int")), Lit(3))
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)
    mods = actual.asInstanceOf[DefDef].mods
    assert(!mods.isPrivate)
    assert(mods.isProtected)
    assert(mods.privateWithin == "core")

    actual = parse("def f[T]: T = ???")
    expect = DefDef(emptyMods, "f",
      TypeParam(emptyMods, "T", Nil, None, Nil) :: Nil,
      Nil, Some(TypeIdent("T")), Ident("???")
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("def f[T](x: T): T = ???")
    expect = DefDef(emptyMods, "f",
      TypeParam(emptyMods, "T", Nil, None, Nil) :: Nil,
      (Param(emptyMods, "x", Some(TypeIdent("T")), None) :: Nil) :: Nil,
      Some(TypeIdent("T")), Ident("???")
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)

    actual = parse("def f(x: Int)(implicit a: A, b: B): Int")
    expect = DefDecl(emptyMods, "f", Nil,
      (Param(emptyMods, "x", Some(TypeIdent("Int")), None) :: Nil) ::
        (Param(emptyMods, "a", Some(TypeIdent("A")), None) ::
          Param(emptyMods, "b", Some(TypeIdent("B")), None) :: Nil) :: Nil,
      TypeIdent("Int")
    )
    // println(s"expect: $expect"); println(s"actual: $actual")
    assert(actual.toString == expect.toString)
    assert(!actual.asInstanceOf[DefDecl].paramss.head.head.mods.isImplicit)
    assert(actual.asInstanceOf[DefDecl].paramss.last.head.mods.isImplicit)
    assert(actual.asInstanceOf[DefDecl].paramss.last.last.mods.isImplicit)

    defn
  }
}