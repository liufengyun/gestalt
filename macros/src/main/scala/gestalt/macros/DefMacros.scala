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

class plus2(a: Any) {
  inline def apply(b: Any): Any = meta {
    q"a + $b"
  }
}

/*object ImplicitsForNumbers {
  implicit class PlusFor(a: Any) {
    inline def plus(b: Any): Any = meta {
//      val q"$_($prefix)" = q"this.a"
//      println(s"!!!$prefix")
      q"a + $b"
    }
  }
}*/

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