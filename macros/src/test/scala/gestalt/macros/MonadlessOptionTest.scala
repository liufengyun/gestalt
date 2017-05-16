import scala.gestalt._
import Decorators._

class MonadlessOptionTest extends TestSuite {
  import MonadlessOption._

  def get[T](t: Option[T]) = t.get

  def fail[T]: T = throw new Exception

  val one = Option(1)
  val two = Option(2)

  test("apply") {
    assert(lift {  1 } === Some(1))
  }

  test("collect") {
    assert(lift { unlift(one) + unlift(two) } === Some(3))
  }


  test("map") {
    assert(lift { unlift(one) + 1 } === Some(2))
  }

  test("flatMap") {
    assert(
      lift {
        val a = unlift(one)
        a + unlift(two)
      } === Some(3)
    )
  }

  test("if") {
    assert(
      lift {
        val a = unlift(one)
        if (unlift(two) > a) unlift(one) + 5
        else a + unlift(two)
      } === Some(6)
    )
  }

  test("if2") {
    assert(
      lift {
        val a = unlift(one)
        if (unlift(two) > a)  5
        else a
      } === Some(5)
    )
  }


  test("if3") {
    assert(
      lift {
        val a = unlift(one)
        if (a > 2)  unlift(two)
        else unlift(one)
      } === Some(1)
    )
  }

  test("deep") {
    val res = lift {
      val a: Int = unlift(Some(2))
      val b = unlift(one) + unlift(two)
      val c = unlift(Some(4))
      unlift(two) * a + b + c
    }

    assert(res === Some(11))
  }

  test("none") {
    val res = lift {
      val a: Int = unlift(None)
      unlift(one) + 1 + a
    }

    assert(res === None)
  }

  test("sequence") {
    val res = lift {
      unlift(one)
      unlift(two)
      unlift(Some(5)) + 8
    }

    assert(res === Some(13))
  }

  test("nested") {
    val res = lift {
      val a = {
        unlift(one)
        unlift(two)
        unlift(Some(5)) + 8
      }

      if (unlift(one) > a) {
        val x = unlift(two)
        unlift(Some(5)) +  x
      }
      else {
        val x = unlift(one)
        val y = unlift(two)
        unlift(Some(5)) + x + y
      }
    }

    assert(res === Some(8))
  }
}
