package scala_cats.chapter01

object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] = (value: String) => JsString(value)

  implicit val personWriter: JsonWriter[Person] = (value: Person) => JsObject(
    Map(
      "name" -> JsString(value.name),
      "email" -> JsString(value.email)
    )
  )

  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] = {
    case Some(aValue) => writer.write(aValue)
    case None => JsNull
  }
}