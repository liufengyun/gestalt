class JsonMacroTest extends TestSuite {
  test("simple case class"){
    case class Name(first: String, last: String)
    val format = JsonMacros.format[Name]()
  }
}