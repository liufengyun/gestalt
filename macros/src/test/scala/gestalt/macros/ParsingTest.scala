import scala.gestalt._

class ParsingTest extends TestSuite {
  test("parse types") {
    @testTypes val x = 3
  }

  test("parse terms") {
    @testTerms val x = 3
  }
}

