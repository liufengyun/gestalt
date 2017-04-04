import scala.gestalt._

object plusObject {
  inline def apply(a: Int, b: Int): Int = meta {
    q"$a + $b"
  }

  inline def defaultArgs(a:Int, b: Int = 1): Int = meta {
    q"$a + $b"
  }
  inline def curried(a:Int)(b: Int): Int = meta {
    q"$a + $b"
  }
  inline def poly(a: Any, b: Int): Int = meta {
    a match {
      case toolbox.Lit(i:Int) => q"$a + $b"
      case toolbox.Lit(s:String) => q"$a.toInt + $b"
      case other =>
        toolbox.error(s"expected String or Interger constants",a)
        toolbox.Lit(null)
    }
  }

  inline def varargs(tped: Int*): Int = meta {
    val q"$items: $_" = tped
    items match {
      case toolbox.SeqLiteral(items:Seq[toolbox.Tree]) =>
        items.reduceLeft((a, b) => q"$a + $b")
      case _ =>
        q"$items.reduce((a:Int,b:Int)=> a + b)"
    }
  }
}


class plus {
  inline def apply(a: Int, b: Int): Int = meta {
    q"$a + $b"
  }
}

object plusOne {
  inline def apply(a: Int): Int =  meta {
    q"$a + 1"
  }
}

class plus2(val a: Int) {
  inline def apply(b: Int): Int = meta {
    q"$this.a + $b"
  }
}

object ImplicitsForNumbers {
  implicit class PlusFor(val a: Int) {
    inline def plus(b: Int): Int = meta {
      q"$this.a + $b"
    }
  }
}

object ImplicitBigInt {
  implicit inline def string2BigInt(s: String): BigInt = meta {
    val toolbox.Lit(str: String) = s
    val bigInt = BigInt(str)
    val radix = Character.MAX_RADIX
    val compressedString = bigInt.toString(radix)
    q"BigInt(${toolbox.Lit(compressedString)},${toolbox.Lit(radix)})"
  }
}

object scope {
  inline def is[T](a: Any): Boolean = meta {
    q"$a.isInstanceOf[$T]"
  }

  inline def both[S, T](a: Any): Boolean = meta {
    q"$a.isInstanceOf[$S] && $a.isInstanceOf[$T]"
  }

  // test nested method inside macro def -- used to be a problem with @static implementation
  inline def mapTest(): Int = meta {
    val sum = (1 to 5).map(_ * 2).sum
    toolbox.Lit(sum)
  }
}

object trees {
  inline def some3(): Option[Int] = meta {
    q"Some(3)"
  }
  inline def five(): Int = meta {
    q"5"
  }
  inline def pi(): Double = meta {
    q"Math.PI"
  }
  inline def ident(a: Any): Any = meta {
    q"$a"
  }
}

object Inheritance {
  trait PlusOne {
    def a: Int
    inline def plus1() = meta {
      q"$this.a + 1"
    }
  }
  class A(val a: Int) extends PlusOne
  object B extends PlusOne {
    val k = 1000
    def a = 8 * k + 1000
  }
  val a39 = new PlusOne {
    def a = 39
  }
}