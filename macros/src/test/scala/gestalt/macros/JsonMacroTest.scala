class JsonMacroTest extends TestSuite {
  test("simple case class"){
    case class Name(first: String, last: String)
    val format = JsonMacros.format[Name]()
    import JsonMacros._
    assert(format.toJson(Name("John","Smith")) == JsObject(Seq(
      "first" -> JsString("John"),
      "last" -> JsString("Smith"))))
  }
}