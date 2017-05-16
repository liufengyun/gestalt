import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq

import scala.gestalt._

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
    import toolbox._

    test("modifiers") {
      val q"$mods object $name" = q"private object A"
      assert(mods.isPrivate)
    }

    test("modifiers of class constructor") {
      val q"$mods1 class $name2 $mods2 ($params)" = q"case class A private[core](x: Int)"
      assert(mods1.isCase)
      assert(mods2.isPrivate)
      assert(mods2.privateWithin === "core")
    }

    test("select") {
      val sel = "hello"
      q"JsString(o.$sel)"
    }

    test("new") {
      val file = Lit("path")
      val expr = q"new xsd($file)"
      assert(expr.toString === NewInstance(None, "xsd", Nil, (Lit("path") :: Nil) :: Nil).toString)
    }

    test("new 2") {
      val expr = q"new X with T { def m = 42 }"
      val anonymClass = NewAnonymClass(
        InitCall(None, "X", Nil, Nil) :: InitCall(None, "T", Nil, Nil) :: Nil,
        None,
        DefDef(emptyMods, "m", Nil, Nil, None, Lit(42)) :: Nil
      )
      assert(expr.toString === anonymClass.toString)
    }

    test("apply") {
      val q"foo($term1, ..$terms, $term2)" = q"foo(x, y, z, q)"
      assert(term1.toString === Ident("x").toString)
      assert(terms.toString === List(Ident("y"), Ident("z")).toString)
      assert(term2.toString === Ident("q").toString)
    }

    test("apply") {
      val term = q"x"
      val terms = List(q"y", q"z")
      val res = Apply(Ident("foo"), Ident("x") :: Ident("y") :: Ident("z") :: Ident("x") :: Nil)
      assert(q"foo($term, ..$terms, $term)".toString === res.toString)
    }

    test("apply") {
      val q"foo($x, ..$ys, $z)" = q"foo(1, 2, 3)"
      assert(x.toString === Lit(1).toString)
      assert(ys.toString === List(Lit(2)).toString)
      assert(z.toString === Lit(3).toString)
    }

    test("apply") {
      val x = q"1"
      val ys = List(q"2")
      val z = q"3"
      val ts = Nil
      val res = Apply(Ident("foo"), Lit(1) :: Lit(2) :: Lit(3) :: Nil)
      assert(q"foo($x, ..$ys, $z, ..$ts)".toString === res.toString)
    }

    test("apply") {
      val q"$expr($name)" = q"foo(bar)"
      assert(expr.toString === Ident("foo").toString)
      assert(name.toString === Ident("bar").toString)
    }

    test("apply") {
      val expr = q"foo"
      val name = q"bar"
      assert(q"$expr($name)".toString === Apply(Ident("foo"), Ident("bar") :: Nil).toString)
    }

    test("apply") {
      val q"$ref[..$tpes](..$apats)" = q"x[A, B](Q, W)"
      assert(ref.toString === Ident("x").toString)
      assert(tpes.toString === List(TypeIdent("A"), TypeIdent("B")).toString)
      assert(apats.toString === List(Ident("Q"), Ident("W")).toString)
    }

    test("apply") {
      val q"$ref[..$tpes](..$apats)" = q"x(Q, W)"
      assert(ref.toString === Ident("x").toString)
      assert(tpes.toString === List().toString)
      assert(apats.toString === List(Ident("Q"), Ident("W")).toString)
    }

    test("select") {
      val q"$expr.$name" = q"foo.bar"
      assert(expr.toString === Ident("foo").toString)
      assert(name === "bar")
    }

    test("select") {
      val expr = q"foo"
      val name = "bar"
      assert(q"$expr.$name".toString === Select(Ident("foo"), "bar").toString)
    }

    test("tuple") {
      val q"(..$terms)" = q"(y, z)"
      assert(terms.toString === List(Ident("y"), Ident("z")).toString)
    }

    test("tuple") {
      val terms = List(q"y", q"z")
      assert(q"(..$terms)".toString === Tuple(Ident("y") :: Ident("z") :: Nil).toString)
    }

    test("ascribe") {
      val exp = q"1"
      val tpe = t"Double"
      assert(q"$exp: $tpe".toString === Ascribe(Lit(1), TypeIdent("Double")).toString)
    }

    test("ascribe") {
      val q"$exp: $tpe" = q"1: Double"
      assert(exp.toString === Lit(1).toString)
      assert(tpe.toString === TypeIdent("Double").toString)
    }

    test("assign") {
      val q"$expr1 = $expr2" = q"foo = bar"
      assert(expr1.toString === Ident("foo").toString)
      assert(expr2.toString === Ident("bar").toString)
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

    test("block") {
      val q"{foo; ..$statz; $astat}" = q"{ foo; val a = x; val b = y; val c = z }"
      assert(statz.toString ===
        List(
          ValDef(emptyMods, "a", None, Ident("x")),
          ValDef(emptyMods, "b", None, Ident("y"))
        ).toString
      )

      assert(astat.toString === ValDef(emptyMods, "c", None, Ident("z")).toString)
    }


    test("block 2") {
      val stats = List(q"val x = 1", q"val y = 2")
      assert(q"{ ..$stats }".toString ===
        Block(
          List(ValDef(emptyMods, "x", None, Lit(1)), ValDef(emptyMods, "y", None, Lit(2)))
        ).toString
      )
    }

    test("if") {
      val q"if ($expr1) $expr2 else $expr3" = q"if (1 > 2) a else b"
      assert(expr1.toString === Infix(Lit(1), ">", Lit(2)).toString)
      assert(expr2.toString === Ident("a").toString)
      assert(expr3.toString === Ident("b").toString)
    }

    test("if2") {
      val expr1 = q"1 > 2"
      val expr2 = q"a"
      val expr3 = q"b"
      assert(q"if ($expr1) $expr2 else $expr3".toString ===
        If(Infix(Lit(1), ">", Lit(2)), Ident("a"), Ident("b")).toString
      )
    }

    test("match") {
      val expr = q"foo match { case bar => baz; case _ => foo }"
      val res = Match(
        Ident("foo"),
        List(
          Case(Ident("bar"), None, Ident("baz")),
          Case(Ident("_"), None, Ident("foo"))
        )
      )
      assert(expr.toString === res.toString)
    }

    /*
    test("function") {
      assert(q"(i: Int) => 42".toString === "Term.Function(Seq(Term.Param(Nil, Term.Name(\"i\"), Some(Type.Name(\"Int\")), None)), Lit(42))")

      val q"(..$paramz) => $expr" = q"(x: Int, y: String) => 42"
      assert(paramz.toString === "List(x: Int, y: String)")
      assert(paramz(0).toString === "Term.Param(Nil, Term.Name(\"x\"), Some(Type.Name(\"Int\")), None)")
      assert(paramz(1).toString === "Term.Param(Nil, Term.Name(\"y\"), Some(Type.Name(\"String\")), None)")
      assert(expr.toString === "Lit(42)")
    }

    test("while") {
      q"while (foo) bar"
    }

    test("do while") {
      q"do foo while (bar)"
    }

    test("for") {
      q"for (a <- as; x <- xs; y <- ys; if bar; b <- bs) foo(x, y)"
    }

    test("for yield") {
      q"for (a <- as; x <- xs; y <- ys; b <- bs) yield foo(x, y)"
    }

    test("try") {
      q"try foo catch { case a => b; case _ => bar; case 1 => 2; case q => w} finally baz"
    }

    test("throw") {
      q"throw new Exception"
    }

    test("return") {
      q"return 3 + 5"
    }

    test("type select") {
      val t"$ref.$tname" = t"X.Y"
      assert(ref.toString === "Term.Name(\"X\")")
      assert(tname.toString === "Type.Name(\"Y\")")
    }

    test("type singleton") {
      val t"$ref.type" = t"X.type"
      assert(ref.toString === "Term.Name(\"X\")")
    }

    test("type applied") {
      val t"$tpe[..$tpes]" = t"X[Y, Z]"
      assert(tpe.toString === "Type.Name(\"X\")")
      assert(tpes.toString === "List(Y, Z)")
      assert(tpes(0).toString === "Type.Name(\"Y\")")
      assert(tpes(1).toString === "Type.Name(\"Z\")")
    }

    test("function type") {
      val t"(..$atpes) => $tpe" = t"(X, Y) => Z"
      assert(atpes.toString === "List(X, Y)")
      assert(atpes(0).toString === "Type.Name(\"X\")")
      assert(atpes(1).toString === "Type.Name(\"Y\")")
      assert(tpe.toString === "Type.Name(\"Z\")")
    }

    test("type tuple") {
      val t"(..$tpes)" = t"(X, Y)"
      assert(tpes.toString === "List(X, Y)")
    }

    test("type refinement") {
      val t"$tpe {..$stats}" = t"A with B with C { val a: A; val b: B }"
    }

    test("annotated type") {
      t"X @a @b"
    }

    test("type bounds") {
      val t"_ >: $tpe1 <: $tpe2" = t"_ >: X <: Y"
    }

    test("by name type") {
      q"def f(x: => Int): Int = 3"
    }

    test("repeated type") {
      q"def f(x: Int*): Int = 3"
    }

    test("pat def") {
    }

    test("seq def") {
    }

    */

    defn
  }
}
