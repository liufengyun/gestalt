import scala.gestalt._

object plusObject {
  def apply(a: Int, b: Int): Int = meta {
    q"$a + $b"
  }

  def defaultArgs(a:Int, b: Int = 1): Int = meta {
    q"$a + $b"
  }
  def curried(a:Int)(b: Int): Int = meta {
    q"$a + $b"
  }
  def poly(a: Any, b: Int): Int = meta {
    a match {
      case toolbox.Lit(i:Int) => q"$a + $b"
      case toolbox.Lit(s:String) => q"$a.toInt + $b"
      case other =>
        toolbox.error(s"expected String or Interger constants",a)
        toolbox.Lit(null)
    }
  }

  def varargs(items: Int*): Int = meta {
    items match {
      case toolbox.SeqLiteral(items: Seq[toolbox.TermTree]) =>
        items.reduceLeft[toolbox.TermTree]((a, b) => q"$a + $b")
      case q"$items: $_" =>
        q"$items.reduce((a:Int,b:Int)=> a + b)"
    }
  }

  def deconstructApply(items: Any): Int = meta {
    items match {
      case toolbox.Apply(prefix: toolbox.Tree, items: Seq[toolbox.TermTree]) =>
        items.reduceLeft[toolbox.TermTree]((a, b) => q"$a + $b")
      case _ =>
        toolbox.error("expected application of Ints",items)
        toolbox.Lit(null)
    }
  }
}


class plus {
  def apply(a: Int, b: Int): Int = meta {
    q"$a + $b"
  }
}

object plusOne {
  def apply(a: Int): Int =  meta {
    q"$a + 1"
  }
}

class plus2(val a: Int) {
  def apply(b: Int): Int = meta {
    q"$this.a + $b"
  }
}

object ImplicitsForNumbers {
  implicit class PlusFor(val a: Int) {
    def plus(b: Int): Int = meta {
      q"$this.a + $b"
    }
  }
}

object ImplicitBigInt {
  implicit def string2BigInt(s: String): BigInt = meta {
    val toolbox.Lit(str: String) = s
    val bigInt = BigInt(str)
    val radix = Character.MAX_RADIX
    val compressedString = bigInt.toString(radix)
    q"BigInt(${toolbox.Lit(compressedString)},${toolbox.Lit(radix)})"
  }
}

object scope {
  def is[T](a: Any): Boolean = meta {
    q"$a.isInstanceOf[$T]"
  }

  def both[S, T](a: Any): Boolean = meta {
    q"$a.isInstanceOf[$S] && $a.isInstanceOf[$T]"
  }

  // test nested method inside macro def -- used to be a problem with @static implementation
  def mapTest(): Int = meta {
    val sum = (1 to 5).map(_ * 2).sum
    toolbox.Lit(sum)
  }
}

object trees {
  def some3(): Option[Int] = meta {
    q"Some(3)"
  }
  def five(): Int = meta {
    q"5"
  }
  def pi(): Double = meta {
    q"Math.PI"
  }
  def ident(a: Any): Any = meta {
    q"$a"
  }
}

object Inheritance {
  trait PlusOne {
    def a: Int
    def plus1() = meta {
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

object Materializer {
  implicit def defaultOpt[T]: Option[T] = meta { q"None" }
  implicit def defaultSome[T](implicit x: T): Some[T] = meta { q"Some($x)" }
}

object Locations {
  def currentLocation(): Location = meta {
    val pos = toolbox.currentLocation
    q"scala.gestalt.Location(${toolbox.Lit(pos.fileName)}, ${toolbox.Lit(pos.line)}, ${toolbox.Lit(pos.column)})"
  }
}

object CaseInfo {
  def fields[T]: List[String] = meta {
    val tp = toolbox.typeOf(T)
    if (!toolbox.isCaseClass(tp)) {
      toolbox.error("Not a case class", T)
      q"scala.Nil"
    }
    else {
      val fieldTrees = toolbox.caseFields(tp).map(m => toolbox.Lit(toolbox.name(m)))
      q"List(..$fieldTrees)"
    }
  }
}

object MultiParamBlocks {
  def f(a: Int)(b: Int): Int = meta {
    q"$a + $b"
  }

  def g(a: Int)(b: Int)(implicit c: Int): Int = meta {
    q"$a + $b - $c"
  }
}
