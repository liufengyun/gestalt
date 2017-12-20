import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq

import scala.gestalt._
import quasiquotes._

import untpd._

object Quasiquotes {
  implicit class TestHelper(lhs: Any) extends AnyVal {
    def ===(rhs: Any) =
      if (lhs == rhs) true
      else {
        println(Console.WHITE + "lhs: " + lhs)
        println(Console.WHITE + "rhs: " + rhs)
        false
      }
  }

  def test(str: String)(f: => Unit) = f
}

import Quasiquotes._

class Quasiquotes extends StaticAnnotation {

  def apply(defn: Any): Any = meta {

    /*
    test("modifiers") {
      val q"$mods object $name" = q"private object A"
      assert(mods.isPrivate)
    }
    */

    /*
    test("modifiers of class constructor") {
      val q"$mods1 class $name2 $mods2 ($params)" = q"case class A private[core](x: Int)"
      assert(mods1.isCase)
      assert(mods2.isPrivate)
      assert(mods2.privateWithin === "core")
    }
    */

    test("select") {
      val sel = "hello"
      q"JsString(o.$sel)"
    }

    test("new") {
      val file = Lit("path")
      val expr = q"new xsd($file)"
      assert(expr.toString === NewInstance(TypeIdent("xsd"), (Lit("path") :: Nil) :: Nil).toString)
    }

    test("new 2") {
      val expr = q"new X with T { def m = 42 }"
      val anonymClass = NewAnonymClass(
        InitCall(TypeIdent("X"), Nil) :: InitCall(TypeIdent("T"), Nil) :: Nil,
        None,
        DefDef(emptyMods, "m", Nil, Nil, None, Lit(42)) :: Nil
      )
      assert(expr.toString === anonymClass.toString)
    }

    /*
    test("apply") {
      val q"foo($term1, ..$terms, $term2)" = q"foo(x, y, z, q)"
      assert(term1.toString === Ident("x").toString)
      assert(terms.toString === List(Ident("y"), Ident("z")).toString)
      assert(term2.toString === Ident("q").toString)
    } */

    test("apply") {
      val term = q"x"
      val terms = List(q"y", q"z")
      val res = Apply(Ident("foo"), Ident("x") :: Ident("y") :: Ident("z") :: Ident("x") :: Nil)
      assert(q"foo($term, ..$terms, $term)".toString === res.toString)
    }

    /*
    test("apply") {
      val q"foo($x, ..$ys, $z)" = q"foo(1, 2, 3)"
      assert(x.toString === Lit(1).toString)
      assert(ys.toString === List(Lit(2)).toString)
      assert(z.toString === Lit(3).toString)
    } */

    test("apply") {
      val x = q"1"
      val ys = List(q"2")
      val z = q"3"
      val ts = Nil
      val res = Apply(Ident("foo"), Lit(1) :: Lit(2) :: Lit(3) :: Nil)
      assert(q"foo($x, ..$ys, $z, ..$ts)".toString === res.toString)
    }

    /*
    test("apply") {
      val q"$expr($name)" = q"foo(bar)"
      assert(expr.toString === Ident("foo").toString)
      assert(name.toString === Ident("bar").toString)
    } */

    test("apply") {
      val expr = q"foo"
      val name = q"bar"
      assert(q"$expr($name)".toString === Apply(Ident("foo"), Ident("bar") :: Nil).toString)
    }

    /*
    test("apply") {
      val q"$ref[..$tpes](..$apats)" = q"x[A, B](Q, W)"
      assert(ref.toString === Ident("x").toString)
      assert(tpes.toString === List(TypeIdent("A"), TypeIdent("B")).toString)
      assert(apats.toString === List(Ident("Q"), Ident("W")).toString)
    }

    test("apply") {
      val q"$ref(..$apats)" = q"x(Q, W)"
      assert(ref.toString === Ident("x").toString)
      assert(apats.toString === List(Ident("Q"), Ident("W")).toString)
    }

    test("select") {
      val q"$expr.$name" = q"foo.bar"
      assert(expr.toString === Ident("foo").toString)
      assert(name === "bar")
    } */

    test("select") {
      val expr = q"foo"
      val name = "bar"
      assert(q"$expr.$name".toString === Select(Ident("foo"), "bar").toString)
    }

    /*
    test("tuple") {
      val q"(..$terms)" = q"(y, z)"
      assert(terms.toString === List(Ident("y"), Ident("z")).toString)
    } */

    test("tuple") {
      val terms = List(q"y", q"z")
      assert(q"(..$terms)".toString === Tuple(Ident("y") :: Ident("z") :: Nil).toString)
    }

    test("ascribe") {
      val exp = q"1"
      val tpe = t"Double"
      assert(q"$exp: $tpe".toString === Ascribe(Lit(1), TypeIdent("Double")).toString)
    }

    /*
    test("ascribe") {
      val q"$exp: $tpe" = q"1: Double"
      assert(exp.toString === Lit(1).toString)
      assert(tpe.toString === TypeIdent("Double").toString)
    } */

    test("assign") {
      assert(q"foo = bar".toString === Assign(Ident("foo"), Ident("bar")).toString)
    }

    test("update") {
      val expr1 = q"foo"
      val aexprs = List(List(q"a", q"b"))
      val expr2 = q"bar"
      val res = Update(Ident("foo"), (Ident("a") :: Ident("b") :: Nil) :: Nil, Ident("bar"))
      assert(q"$expr1(...$aexprs) = $expr2".toString === res.toString)
    }

    /*
    test("update 2") {
      val q"f($q, y: $_) = $r" = q"f(x: X, y: Y) = 1"
      assert(q.toString === Ident("x".toString))
      assert(r.toString === Lit(1).toString)
    } */

    test("apply 3") {
      val q = q"x: X"
      val r = q"1"
      val res = Update(
        Ident("f"),
        (Ascribe(Ident("x"), TypeIdent("X")) :: Ascribe(Ident("y"), TypeIdent("Y")) :: Nil) :: Nil,
        Lit(1)
      )
      assert(q"f($q, y: Y) = $r".toString === res.toString)
    }

    /*
    test("block") {
      val q"{foo; ..$statz; $astat}" = q"{ foo; val a = x; val b = y; val c = z }"
      assert(statz.toString ===
        List(
          ValDef(emptyMods, "a", None, Ident("x")),
          ValDef(emptyMods, "b", None, Ident("y"))
        ).toString
      )

      assert(astat.toString === ValDef(emptyMods, "c", None, Ident("z")).toString)
    } */


    test("block 2") {
      val stats = List(q"val x = 1", q"val y = 2")
      assert(q"{ ..$stats }".toString ===
        Block(
          List(ValDef(emptyMods, "x", None, Lit(1)), ValDef(emptyMods, "y", None, Lit(2)))
        ).toString
      )
    }

    /*
    test("if") {
      val q"if ($expr1) $expr2 else $expr3" = q"if (1 > 2) a else b"
      assert(expr1.toString === Infix(Lit(1), ">", Lit(2)).toString)
      assert(expr2.toString === Ident("a").toString)
      assert(expr3.toString === Some(Ident("b")).toString)
    } */

    test("if2") {
      val expr1 = q"1 > 2"
      val expr2 = q"a"
      val expr3 = q"b"
      assert(q"if ($expr1) $expr2 else $expr3".toString ===
        If(Infix(Lit(1), ">", Lit(2)), Ident("a"), Ident("b")).toString
      )
    }

    test("match") {
      val expr =
        q"""foo match {
          case foo(x, y) => 3
          case a @ bar(x : T) => 4
          case bar => baz
          case _ => foo
        }"""
      val res = Match(
        Ident("foo"),
        List(
          Case(Pat.Unapply(Ident("foo"), Pat.Var("x") :: Pat.Var("y") :: Nil), None, Lit(3)),
          Case(Pat.Bind("a", Pat.Unapply(Ident("bar"), Pat.Ascribe("x", TypeIdent("T")) :: Nil)), None, Lit(4)),
          Case(Pat.Var("bar"), None, Ident("baz")),
          Case(Pat.Var("_"), None, Ident("foo"))
        )
      )
      assert(expr.toString === res.toString)
    }


    test("function") {
      assert(q"(i: Int) => 42".toString ===
        Function(Param("i", TypeIdent("Int")) :: Nil, Lit(42)).toString
      )
    }

    test("function2") {
      val expr = q"(x: Int, y: String) => 42"
      assert(expr.toString ===
        Function(
          Param("x", TypeIdent("Int")) :: Param("y", TypeIdent("String")) :: Nil,
          Lit(42)
        ).toString
      )
    }

    test("while") {
      assert(q"while (foo) bar".toString === While(Ident("foo"), Ident("bar")).toString)
    }

    test("do while") {
      assert(q"do foo while (bar)".toString === DoWhile(Ident("foo"), Ident("bar")).toString)
    }

    test("for") {
      val expr = q"for (a <- as; x <- xs; y <- ys; if bar; b <- bs) foo(x, y)"
      val res = For.ForDo(
        For.GenFrom(Pat.Ident("a"), Ident("as")) ::
          For.GenFrom(Pat.Ident("x"), Ident("xs")) ::
          For.GenFrom(Pat.Ident("y"), Ident("ys")) ::
          For.Guard(Ident("bar")) ::
          For.GenFrom(Pat.Ident("b"), Ident("bs")) :: Nil,
        Apply(Ident("foo"), List(Ident("x"), Ident("y")))
      )
      assert(expr.toString === res.toString)
    }

    test("for yield") {
      val expr = q"for (a <- as; x <- xs; y <- ys; b <- bs) yield foo(x, y)"
      val res = For.ForYield(
        For.GenFrom(Pat.Ident("a"), Ident("as")) ::
          For.GenFrom(Pat.Ident("x"), Ident("xs")) ::
          For.GenFrom(Pat.Ident("y"), Ident("ys")) ::
          For.GenFrom(Pat.Ident("b"), Ident("bs")) :: Nil,
        Apply(Ident("foo"), List(Ident("x"), Ident("y")))
      )
      assert(expr.toString === res.toString)
    }

    test("try") {
      val expr = q"try foo catch { case a => b } finally baz"
      val res = Try(
        Ident("foo"),
        Case(Pat.Ident("a"), None, Ident("b")) :: Nil,
        Some(Ident("baz"))
      )
      assert(expr.toString === res.toString)
    }

    test("throw") {
      assert(q"throw s".toString === Throw(Ident("s")).toString)
    }

    test("return") {
      assert(q"return 8".toString === Return(Lit(8)).toString)
    }

    test("type select") {
      val expr = t"x.Y"
      assert(expr.toString === TypeSelect(Ident("x"), "Y").toString)
    }

    test("type singleton") {
      val expr = t"x.type"
      assert(expr.toString === TypeSingleton(Ident("x")).toString)
    }

    test("type applied") {
      val expr = t"X[Y, Z]"
      val res = TypeApply(TypeIdent("X"), List(TypeIdent("Y"), TypeIdent("Z")))
      assert(expr.toString === res.toString)
    }


    test("function type") {
      val expr = t"(X, Y) => Z"
      val res = TypeFunction(List(TypeIdent("X"), TypeIdent("Y")), TypeIdent("Z"))
      assert(expr.toString === res.toString)
    }

    test("type tuple") {
      val expr = t"(X, Y)"
      val res = TypeTuple(List(TypeIdent("X"), TypeIdent("Y")))
      assert(expr.toString === res.toString)
    }

    test("type refinement") {
      val expr = t"A { val a: A }"
      val res = TypeRefine(TypeIdent("A"), List(ValDecl(emptyMods, "a", TypeIdent("A"))))
      assert(expr.toString === res.toString)
    }

    /*
    test("annotated type") {
      t"X @a @b"
    }
    */

    test("by name type") {
      val expr = q"def f(x: => Int): Int = 3"
      val res = DefDef(emptyMods, "f", Nil,
        (Param("x", TypeByName(TypeIdent("Int"))) :: Nil) :: Nil,
        Some(TypeIdent("Int")),
        Lit(3)
      )
      assert(expr.toString === res.toString)
    }

    test("repeated type") {
      val expr = q"def f(x: Int*): Int = 3"
      val res = DefDef(emptyMods, "f", Nil,
        (Param("x", TypeRepeated(TypeIdent("Int"))) :: Nil) :: Nil,
        Some(TypeIdent("Int")),
        Lit(3)
      )
      assert(expr.toString === res.toString)

    }

    test("var def") {
      val term = q"var x = 3"
      val res = ValDef(emptyMods.setMutable, "x", None, Lit(3))
      assert(term.toString === res.toString)
    }

    test("pat def") {
      val expr = q"val f(x) = a"
      val res = PatDef(emptyMods, Pat.Unapply(Ident("f"), Pat.Var("x") :: Nil), None, Ident("a"))
      assert(expr.toString === res.toString)
    }

    /*
    test("seq def") {
      val expr = q"val x, y : Int = 3"
      val res = SeqDef(emptyMods, "x" :: "y" :: Nil, Some(TypeIdent("Int")), Lit(3))

      assert(expr.toString === res.toString)
    } */

    test("type def") {
      val expr = q"type T = Int"
      val res = TypeAlias(emptyMods, "T", Nil, TypeIdent("Int"))
      assert(expr.toString === res.toString)
    }

    test("type decl") {
      val expr = q"type T >: A <: B"
      val bound = TypeBounds(Some(TypeIdent("A")), Some(TypeIdent("B")))
      val res = TypeDecl(emptyMods, "T", Nil, Some(bound))
      assert(expr.toString === res.toString)
    }

    defn
  }
}
