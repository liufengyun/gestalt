class DefMacroTest extends TestSuite {
  test("plusObject") {
    assert(plusObject(3, 5) == 8)
  }
  test("plus") {
    val p = new plus
    assert(p(3, 5) == 8)
  }

  test("plusOne") {
    assert(plusOne(4) == 5)
  }
  test("plus2") {
    val p = new plus2(3)
    assert(p(5) == 8)
  }

  test("plus2 on method call") {
    case class Holder(item: plus2)
    val holder = Holder(new plus2(3))
    assert(holder.item.apply(5) == 8)

    object Generator {
      def fromSeed(seed: Int) = new plus2(seed + 1)
      def generate = new plus2(42)
    }
    assert(Generator.fromSeed(20).apply(3) == 24)
    assert(Generator.generate.apply(0) == 42)

    assert(new plus2(1)(2 + 3) == 6)
    val five = 5
    assert(new plus2(1)(five) == 6)
  }

  test("plus2 on expression") {
    val three = 3
    assert(new plus2(3)(5) == 8)
    assert(new plus2(three * 1)(5) == 8)
  }

  test("plus from class inside class") {
    import packaged.macros.InnerClassMacro
    val outer = new InnerClassMacro()
    assert(outer.createInner.plus(5, 3) == 8)
  }

  test("plus from object inside class") {
    import packaged.macros.InnerClassMacro
    val outer = new InnerClassMacro()
    assert(outer.InnerObject.plus(5, 3) == 8)
  }

  test("plus from object inside object") {
    import packaged.macros.InnerObjectMacro
    assert(InnerObjectMacro.InnerObject.plus(5, 3) == 8)
  }

  test("plus from class inside object") {
    import packaged.macros.InnerObjectMacro
    assert(new InnerObjectMacro.Inner().plus(5, 3) == 8)
  }

  test("plus2 on option get") {
    val option = Some(new plus2(3))
    assert(option.get.apply(5) == 8)
  }

  test("implicit plus") {
    import ImplicitsForNumbers._
    assert(3.plus(5) == 8)
  }

  test("implicit plus from a package") {
    import packaged.macros.ImplicitsForNumbers._
    assert(3.plus(5) == 8)
  }

  test("implicit plus from a package object") {
    import packaged.macros2._
    assert(3.plus(5) == 8)
    assert(plus(3,5) == 8)
    assert(PlusObj.plus(3,5) == 8)
  }

  test("def with type parameters") {
    assert(scope.is[String]("hello"))
    assert(!scope.both[String, List[Int]]("hello"))
  }

  test("explicit big int") {
    import ImplicitBigInt._
    assert(string2BigInt("19") == BigInt(19))
    assert(string2BigInt("19").modPow(exp = 2, 4) == BigInt(1))
  }

  test("implict big int"){
    import ImplicitBigInt._
    assert("19".modPow(exp = 2, 4) == BigInt(1))
  }

  test("constant quasiqoutes") {
    assert(trees.five() == 5)
    assert(trees.some3() == Some(3))
    assert(trees.pi() == Math.PI)
  }

  test("nested method inside macro def") {
    assert(scope.mapTest() == 30)
  }

  test("macro defined in a trait") {
    import Inheritance._
    assert(new A(3).plus1() == 4)
    assert(new A(-1).plus1() == 0)
    assert(B.plus1() == 9001)
    assert(a39.plus1() == 40)
  }
}
