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

  import cats.Eq
  import cats.syntax.eq._

  implicit def eqJson: Eq[Json] = Eq.instance[Json] { (j1, j2) =>
    (j1, j2) match {
      case (JsObject(m1), JsObject(m2)) => m1 === m2
      case (JsString(s1), JsString(s2)) => s1 === s2
      case (JsNumber(d1), JsNumber(d2)) => d1 === d2
      case (JsNull, JsNull) => true
      case _ => false
    }
  }
}

// Enable type class use via extension method
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    // This is the extension method
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}