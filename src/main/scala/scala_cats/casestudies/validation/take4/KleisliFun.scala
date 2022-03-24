package scala_cats.casestudies.validation.take4

import cats.data.Kleisli

object KleisliFun {
  // These steps each transform an input Int into an output of type List[Int]:

  // Kleisli represents a function A => F[B]
  // final case class Kleisli[F[_], -A, B](run: A => F[B])

  val step1: Kleisli[List, Int, Int] = Kleisli(x => List(x + 1, x - 1))

  val step2: Kleisli[List, Int, Int] = Kleisli(x => List(x, -x))

  val step3: Kleisli[List, Int, Int] = Kleisli(x => List(x * 2, x / 2))
}
