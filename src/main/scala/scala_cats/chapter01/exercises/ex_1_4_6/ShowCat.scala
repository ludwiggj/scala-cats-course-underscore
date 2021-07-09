package scala_cats.chapter01.exercises.ex_1_4_6

import cats.instances.int._
import cats.instances.string._
import cats.Show
import cats.syntax.show._

final case class ShowCat(name: String, age: Int, colour: String)

object ShowCat {
  // Automatically in scope as it is in companion object

  implicit val showCat: Show[ShowCat] = Show.show(cat =>
    s"${cat.name.show} is a ${cat.age.show} year-old ${cat.colour.show} cat"
  )
}