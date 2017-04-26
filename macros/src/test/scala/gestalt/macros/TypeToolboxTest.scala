import scala.gestalt._


class TypeToolboxTest extends TestSuite {
  import TypeToolbox._


  type Age = Int

  test("=:=") {
    assert(=:=[Nil.type, Nil.type])
    assert(=:=[Int, Int])
    assert(! =:=[Int, String])
    assert(=:=[Int, Age])
  }

  test("<:<") {
    assert(<:<[Int, Int])
    assert(<:<[Age, Int])
    assert(<:<[None.type, Option[Int]])
    assert(<:<[Nil.type, List[Int]])
    assert(! <:<[Int, String])

    val a = 5
    assert(<:<[3, Int])
    assert(<:<[a.type, Int])
  }

  test("typeRef") {
    assert(typeRef[String]("java.lang.String"))
    assert(typeRef[String]("scala.Predef.String"))
    assert(typeRef[Int]("scala.Int"))
    assert(typeRef[Boolean]("scala.Boolean"))
  }

  test("termRef") {
    assert(termRef[None.type]("scala.None"))
    assert(termRef[Nil.type]("scala.Nil"))
  }

  test("isCaseClass") {
    case class Student(name: String, age: Int)
    class Teacher(name: String, age: Int)
    trait Staff
    assert(isCaseClass[Student])
    assert(!isCaseClass[Teacher])
    assert(!isCaseClass[Staff])

  }

  test("caseFields") {
    case class Student(name: String, age: Int)
    case class Teacher(name: String, age: Int) {
      val school: String = "EPFL"
      var salary: Int = 20000
    }

    assert(caseFields[Student] == List("name", "age"))
    assert(caseFields[Teacher] == List("name", "age"))
  }

  test("asSeenFrom") {
    case class M[T](x: T)
    val a = new M(4)

    assert(asSeenFrom[a.type, Int]("x"))


    trait Base {
      val x: InBase
      trait InBase
    }

    class Child extends Base {
      val x: Inner = new Inner
      class Inner extends InBase
    }

    val m = new Child
    assert(asSeenFrom[m.type, m.Inner]("x"))

    trait Box {
      type T
      val x: T
    }
    class InBox extends Box {
      type T = Int
      val x = 3
    }
    val box = new InBox
    assert(asSeenFrom[box.type, Int]("x"))
  }

  test("fields") {
    trait Base {
      val x = 3

      def f = 4
    }

    class Derived extends Base {
      val y = 3

      def g = 3
    }

    assert(fieldIn[Base]("x").nonEmpty)
    assert(fieldIn[Derived]("x").isEmpty)
    assert(fieldIn[Derived]("y").nonEmpty)
    assert(fieldsIn[Base].size == 1)
    assert(fieldsIn[Base].head == "x")
    assert(fieldsIn[Derived].size == 1)
    assert(fieldsIn[Derived].head == "y")
  }

  test("methods") {
     trait Base {
      val x = 3

      def f = 4
    }

    class Derived extends Base {
      val y = 3

      def g = 3
    }

    assert(method[Base]("f").nonEmpty)
    assert(method[Base]("x").isEmpty)
    assert(methodIn[Base]("f").nonEmpty)
    assert(methodIn[Base]("x").isEmpty)
    assert(methodsIn[Base].size == 1)
    assert(methodsIn[Base].head == "f")

    assert(method[Derived]("f").nonEmpty)
    assert(method[Derived]("g").nonEmpty)
    assert(method[Derived]("y").isEmpty)
    assert(methodIn[Derived]("f").isEmpty)
    assert(methodIn[Derived]("g").nonEmpty)
    assert(methodIn[Derived]("x").isEmpty)
    assert(methodsIn[Derived].size == 1)
    assert(methodsIn[Derived].head == "g")
  }
}
