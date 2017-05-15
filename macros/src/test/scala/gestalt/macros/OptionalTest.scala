import scala.gestalt._
import Decorators._

class OptionalTest extends TestSuite {
  test("getOrElse") {
    val opt = new Optional(4)
    assert(opt.getOrElse(3) === 4)

    val opt2 = new Optional(null)
    assert(opt2.getOrElse(3) === 3)
  }

  /*
  test("map") {
    val opt = new Optional(4)
    assert(opt.map(_ * 2) === new Optional(8))

    val opt2 = new Optional(null)
    assert(opt2.map(_ * 2) === new Optional(null))
  } */
}
