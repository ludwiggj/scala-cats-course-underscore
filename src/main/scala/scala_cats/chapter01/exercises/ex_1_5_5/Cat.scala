package scala_cats.chapter01.exercises.ex_1_5_5

final case class Cat(name: String, age: Int, colour: String)

object Cat {
  import cats.instances.string._
  import cats.instances.int._
  import cats.Eq
  import cats.syntax.eq._

  implicit val eqCat: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
    cat1.name === cat2.name &&
      cat1.age === cat2.age &&
      cat1.colour == cat2.colour
  }
}