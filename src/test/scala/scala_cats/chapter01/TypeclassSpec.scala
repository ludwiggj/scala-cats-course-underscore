package scala_cats.chapter01

import scala_cats.UnitSpec

class TypeclassSpec extends UnitSpec {
  val name = "Graeme Ludwig"
  val email = "graeme.ludwig@itv.com"

  "person jsonWriter" can "work via interface object" in {
    import JsonWriterInstances._
    assert(Json.toJson(Person(name, email)) == JsObject(Map("name" -> JsString(name), "email" -> JsString(email))))
  }

  it can "work via syntax" in {
    import JsonWriterInstances._
    import JsonSyntax._
    assert(Person(name, email).toJson == JsObject(Map("name" -> JsString(name), "email" -> JsString(email))))
  }

  "option jsonWriter" can "write van Option[String]" in {
    import JsonWriterInstances._

    assert(Json.toJson(Option("hello")) == JsString("hello"))
  }

  it can "write van Option[Person]" in {
    import JsonWriterInstances._

    assert(Json.toJson(Person(name, email)) == JsObject(Map("name" -> JsString(name), "email" -> JsString(email))))
  }
}