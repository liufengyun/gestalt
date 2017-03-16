import scala.collection.immutable.Seq

import scala.gestalt._

object plusObject {
  inline def apply(a: Any, b: Any): Any = meta {
    q"$a + $b"
  }
}


class plus {
  inline def apply(a: Any, b: Any): Any = meta {
    q"$a + $b"
  }
}

object plusOne {
  inline def apply(a: Int): Int =  meta {
    q"$a + 1"
  }
}

class plus2(val a: Int) {
  inline def apply(b: Int): Any = meta {
    q"$this.a + $b"
  }
}

object ImplicitsForNumbers {
  implicit class PlusFor(val a: Int) {
    inline def plus(b: Int): Any = meta {
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