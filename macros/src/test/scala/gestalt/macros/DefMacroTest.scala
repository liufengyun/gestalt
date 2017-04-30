class DefMacroTest extends TestSuite {
  test("plusObject") {
    assert(plusObject(3, 5) == 8)

    assert(plusObject.defaultArgs(3, 5) == 8)
    assert(plusObject.defaultArgs(3) == 4)

    assert(plusObject.curried(3)(5) == 8)
//    val c3 = plusObject.curried(3)
//    assert(c3(5) == 8)

    assert(plusObject.poly(3, 5) == 8)
    assert(plusObject.poly("3", 5) == 8)

    val five = 5
    assert(plusObject.varargs(1, 1 + 1, five) == 8)
    assert(plusObject.varargs(1, 2, 5) == 8)
//    assert(plusObject.varargs(Seq(1, 2, 5):_*) == 8)
    val ints = Seq(1, 2, 5)
    assert(plusObject.varargs(ints:_*) == 8)
    assert(plusObject.varargs(3, 5) == 8)
    assert(plusObject.varargs(8) == 8)
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

  test("plus2 on code block") {
    assert({
      val two = 2
      new plus2(1 + two)
    }.apply(5) == 8)
    assert(new plus2(3).apply {
      val twenty = 10 + 5 + 5
      twenty / 4
    } == 8)
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
    import packaged.macros._
    assert(3.plus(5) == 8)
    assert(plus(3,5) == 8)
    assert(PlusObj.plus(3,5) == 8)
  }

  test("deconstructApply") {
    import plusObject.deconstructApply
    def fun(x: Int, y: Int, z: Int) = ???
    assert(deconstructApply(Seq(1, 2, 1, 3)) == 7)
    assert(deconstructApply(List[Int](1, 2, 1, 3)) == 7)
    assert(deconstructApply(fun(1, 2, 4)) == 7)
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

  test("materializer explicit") {
    import Materializer._
    assert(defaultOpt[Int] == None)
  }

  test("materializer implicit") {
    import Materializer._
    assert(implicitly[Option[Int]] == None)

    implicit val defaultName = "test"
    assert(implicitly[Some[String]] == Some("test"))
  }

  test("location") {
    import Locations._
    val pos = currentLocation()
    assert(pos.fileName == "DefMacroTest.scala")
    assert(pos.line == 171) // starts from 0
  }

  test("case info") {
    import CaseInfo._
    case class Student(name: String, age: Int)
    assert(fields[Student] == List("name", "age"))

    // compile-time error message: Not a case class
    // class Teacher(name: String, age: Int)
    // assert(fields[Teacher] == List())
  }

  test("mutiple param blocks") {
    import MultiParamBlocks._
    assert(f(1)(2) == 3)

    implicit val x: Int = 1
    assert(g(1)(2) == 2)
  }
}
