package scala_cats.chapter01.exercises.ex_1_3

object PrintableInstances {
  implicit val printableString: Printable[String] = (value: String) => value
  implicit val printableInt: Printable[Int] = (value: Int) => value.toString
}
