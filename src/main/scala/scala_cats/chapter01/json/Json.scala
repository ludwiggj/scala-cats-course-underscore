package scala_cats.chapter01.json

// Define a very simple JSON AST
sealed trait Json

final case class JsObject(get: Map[String, Json]) extends Json

final case class JsString(get: String) extends Json

final case class JsNumber(get: Double) extends Json

final case object JsNull extends Json

// Enable type class use via interface object
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
}

// Enable type class use via extension method
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    // This is the extension method
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}