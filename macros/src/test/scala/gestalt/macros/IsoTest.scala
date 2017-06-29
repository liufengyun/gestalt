import scala.gestalt._
import Decorators._

class IsoTest extends TestSuite {
  case class Foo(i: Int, s: String, b: Boolean)
  def conv[C, L](c: C)(implicit iso: Iso[C, L]): L = iso.to(c)
  type T = (Int, String, Boolean)

  test("iso explicit") {
    val conv = Iso.materializeIso[Foo, T]
    assert(conv.to(Foo(23, "foo", true)) == (23, "foo", true))
  }

  test("iso") {
    val equiv: T = conv(Foo(23, "foo", true))
    assert(equiv == (23, "foo", true))
  }
}