package scala_cats.chapter07.ex_7_2_2_3

import scala_cats.chapter07.TraverseWorkout.listTraverse
import cats.data.Validated
import cats.instances.list._ // For Monoid

object TraverseValidated {
  type ErrorsOr[A] = Validated[List[String], A]

  def process(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs)(n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      })
}
