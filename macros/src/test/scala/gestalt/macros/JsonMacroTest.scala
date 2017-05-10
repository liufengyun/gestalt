class JsonMacroTest extends TestSuite {
  test("simple case class"){
    case class Name(first: String, last: String)
    val format = JsonMacros.format[Name]()
    import JsonMacros._
    assert(format.toJson(Name("John","Smith")) == JsObject(Seq(
      "first" -> JsString("John"),
      "last" -> JsString("Smith"))))

    val tom = Name("Tom","Sawyer")
    assert(format.fromJson(format.toJson(tom)) == Some(tom))
    assert(format.fromJson(JsObject(Nil)) == None)
  }

  test("composition test"){
    case class Name(first: String, last: String)
    case class Registration(name: Name, email: String)

    implicit val nameFormat = JsonMacros.format[Name]()
    val registrationFormat = JsonMacros.format[Registration]()
    import JsonMacros._

    val registration = Registration(Name("Tom","Sawyer"),"tom.s@mark.twain.book")
    val expectedJson = JsObject(Seq(
      "name" -> JsObject(Seq(
        "first" -> JsString("Tom"),
        "last" -> JsString("Sawyer"))),
      "email" -> JsString("tom.s@mark.twain.book")
      )
    )
    assert(registrationFormat.toJson(registration) == expectedJson)
    assert(registrationFormat.fromJson(registrationFormat.toJson(registration)) == Some(registration))
  }
}