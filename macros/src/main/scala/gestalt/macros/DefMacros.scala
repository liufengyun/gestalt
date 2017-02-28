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

/*
object scope {
  inline def is[T](a: Any): Any = meta {
    q"$a.isInstanceOf[$T]"
  }

  inline def both[S, T](a: Any): Any = meta {
    q"$a.isInstanceOf[$S] && $a.isInstanceOf[$T]"
  }
}
*/