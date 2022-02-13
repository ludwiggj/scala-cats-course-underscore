package scala_cats.chapter07

import cats.Eval
import cats.Foldable

object FoldableWorkout {
  private def bigData: LazyList[Int] =  (1 to 100000).to(LazyList)
  def unsafeSum: Long = bigData.foldRight(0L)(_ + _)
  def safeSum: Eval[Long] = Foldable[LazyList].foldRight(bigData, Eval.now(0L))((num, eval) => eval.map(_ + num))
}