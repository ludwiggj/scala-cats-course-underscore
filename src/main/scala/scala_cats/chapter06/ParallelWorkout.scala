package scala_cats.chapter06

import cats.arrow.FunctionK

object ParallelWorkout {
  object optionToList extends FunctionK[Option, List] {
    def apply[A](fa: Option[A]): List[A] =
      fa match {
        case None => List.empty[A]
        case Some(a) => List(a)
      }
  }
}
