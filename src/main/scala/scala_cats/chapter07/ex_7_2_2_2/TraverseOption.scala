package scala_cats.chapter07.ex_7_2_2_2

import scala_cats.chapter07.TraverseWorkout.listTraverse
import cats.instances.option._

object TraverseOption {
  def process(inputs: List[Int]): Option[List[Int]] =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)
}
