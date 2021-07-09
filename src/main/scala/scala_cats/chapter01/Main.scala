package scala_cats.chapter01

object Main {
  private def summon(): Unit = {
    // summon an instance implicitly
    import JsonWriterInstances._
    implicitly[JsonWriter[String]]
    ()
  }

  def writePersonOption(): Unit = {
    // implicit conversion - don't do this! (no warning)
    implicit def optionWriter[A](writer: JsonWriter[A]): JsonWriter[Option[A]] = {
      case Some(aValue) => writer.write(aValue)
      case None => JsNull
    }

    def displayOptionAsJson[A](aValue: Option[A])(implicit writer: JsonWriter[Option[A]]): Unit = {
      println(writer.write(aValue))
    }

    val name = "Graeme Ludwig"
    val email = "graeme.ludwig@itv.com"

    implicit val personOptionWriter: JsonWriter[Option[Person]] = JsonWriterInstances.personWriter

    displayOptionAsJson(Option(Person(name, email)))
  }

  def implicitConversionsExample(): Unit = {
    // implicit conversion - don't do this! (no warning)
    implicit def stringToInt(s: String): Int = s.length

    val i: Int = "bobby"
    println(i)
  }

  def main(args: Array[String]): Unit = {
    summon()
    writePersonOption()
    implicitConversionsExample()
  }
}