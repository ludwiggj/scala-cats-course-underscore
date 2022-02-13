package scala_cats.chapter07.ex_7_1_2

import cats.Monoid

object Folder {
  def foldLeft[A](l: List[A]): List[A] = l.foldLeft(List.empty[A])((la, a) => a :: la)
  def foldRight[A](l: List[A]): List[A] = l.foldRight(List.empty[A])((a, la) => a :: la)

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    l.foldRight(List.empty[B])((a, lb) => f(a) :: lb)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    l.foldRight(List.empty[B])((a, lb) => f(a) ++ lb)
  }

  def filter[A](l: List[A])(p: A => Boolean): List[A] = {
    l.foldRight(List.empty[A])((a, la) => if (p(a)) a :: la else la)
  }

  def sum(l: List[Int]): Int = {
    l.foldRight(0)(_ + _)
  }

  def sum2[A](l: List[A])(implicit m: Monoid[A]): A = {
    l.foldRight(m.empty)(m.combine)
  }

  def sum3[A](l: List[A])(implicit n: Numeric[A]): A = {
    l.foldRight(n.zero)(n.plus)
  }
}
