import scala.collection.immutable.Seq

import dotty.tools._
import dotc.core.Contexts._

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
  }
}
