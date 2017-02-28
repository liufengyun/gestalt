class DefMacroTest extends TestSuite {
  test("plusObject") {
    assert(plusObject(3, 5) == 8)
  }
  test("plus") {
    val p = new plus
    assert(p(3, 5) == 8)
  }
  test("plus2") {
    val p = new plus2(3)
    assert(p(5) == 8)
  }
  /*  test("implicit plus") {
      import ImplicitsForNumbers._
      assert(3.plus(5) == 8)
    }*/
  /*
    test("def with type parameters") {
      assert(scope.is[String]("hello"))
      assert(!scope.both[String, List[Int]]("hello"))
    } */
}
