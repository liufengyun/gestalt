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
  /*
  test("flatMap") {
    assert(
      lift {
        val a = unlift(one)
        a + unlift(two)
      } === Some(3)
    )
  } */
}
