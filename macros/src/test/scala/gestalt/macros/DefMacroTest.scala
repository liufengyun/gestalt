class DefMacroTest extends TestSuite {
  test("plusObject") {
    assert(plusObject(3, 5) == 8)
  }
  test("plus") {
    val p = new plus
    assert(p(3, 5) == 8)
  }
  /*
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

    test("def with type parameters") {
      assert(scope.is[String]("hello"))
      assert(!scope.both[String, List[Int]]("hello"))
    } */
}
