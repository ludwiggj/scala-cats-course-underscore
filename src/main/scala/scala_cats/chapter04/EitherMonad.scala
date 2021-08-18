package scala_cats.chapter04

import cats.implicits.catsSyntaxEitherId

object EitherMonad {
  def countPositive(nums: List[Int]): Either[String, Int] = {
    nums.foldLeft(0.asRight[String]) {
      case (accumulator, num) => if (num > 0) accumulator.map(_ + 1) else Left("Negative number - abort!")
    }
  }
}
