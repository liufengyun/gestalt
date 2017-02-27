class AnnotationMacroTest extends TestSuite {
  test("main") {
    @main object Test {
      "hello world!"
    }

    assert(Test.stub(null) == "hello world!")
  }

  /*
  test("data") {
    @data class Point(x: Int, y: Int)

    val p = Point(40, 2)
    assert(p.x == 40 && p.y == 2)
  }

  test("xsd") {
    @xsd("macros/tests/schema.xsd")
    object schema

    import schema._
    val note = new Note("Vassily", "Pupkin", "hello", "This is a test!")
  }

  test("cache") {
    import scala.util.Random

    @cache
    def rand(seed: Int): Random = new Random(seed)

    assert(_rand == null)
    val rand1 = rand(3)
    assert(rand1 != null)
    assert(_rand != null)

    val rand2 = rand(4)
    assert(rand1 == rand2)
  }

  // need to put at top-level for indexing to make them available for @dynamic
  case class Authorized(val i: Int)
  case class Token(val i: Int)

  test("dynamic") {

    @dynamic[Authorized]
    def access(name: String) = dynamicParam0.i

    assert(access("hello")(new Authorized(56)) == 56)

    @dynamic[Authorized & Token]
    def visit(name: String) = dynamicParam0.i + dynamicParam1.i

    assert(visit("hello")(new Authorized(5), new Token(10)) == 15)
  }
*/
}
