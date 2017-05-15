import scala.gestalt._
import Decorators._

class OptionalTest extends TestSuite {
  test("getOrElse") {
    val opt = new Optional[String]("hello")
    assert(opt.getOrElse("world") === "hello")

    val opt2 = new Optional[String](null)
    assert(opt2.getOrElse("hello") === "hello")
  }

  test("map") {
    val opt = new Optional[String]("hello")
    assert(opt.map(_ + " world") === new Optional("hello world"))

    val opt2 = new Optional[String](null)
    assert(opt2.map(_ + " world") === new Optional(null))
  }
}
