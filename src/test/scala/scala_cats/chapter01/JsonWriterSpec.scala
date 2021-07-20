package scala_cats.chapter01

import scala_cats.UnitSpec
import scala_cats.chapter01.json.{JsObject, JsString, Json, Person}

class JsonWriterSpec extends UnitSpec {
  private val name = "Graeme Ludwig"
  private val email = "graeme.ludwig@itv.com"

  // TODO - Figure out how to use Eq in this situation - may not be possible / advisable
  "person jsonWriter" can "work via interface object" in {
    import scala_cats.chapter01.json.JsonWriterInstances._
    assert(Json.toJson(Person(name, email)) === JsObject(Map("name" -> JsString(name), "email" -> JsString(email))))
  }

  it can "work via syntax" in {
    import scala_cats.chapter01.json.JsonWriterInstances._
    import scala_cats.chapter01.json.JsonSyntax._
    assert(Person(name, email).toJson === JsObject(Map("name" -> JsString(name), "email" -> JsString(email))))
  }

  "option jsonWriter" can "write van Option[String]" in {
    import scala_cats.chapter01.json.JsonWriterInstances._

    assert(Json.toJson(Option("hello")) === JsString("hello"))
  }

  it can "write van Option[Person]" in {
    import scala_cats.chapter01.json.JsonWriterInstances._

    assert(Json.toJson(Person(name, email)) === JsObject(Map("name" -> JsString(name), "email" -> JsString(email))))
  }
}