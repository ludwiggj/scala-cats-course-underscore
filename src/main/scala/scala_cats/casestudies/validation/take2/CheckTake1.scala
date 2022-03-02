package scala_cats.casestudies.validation.take2

import cats.data.Validated

sealed trait CheckTake1[E, A, B] { self =>
  def apply(a: A): Validated[E, B] = f(a)

  def f(a: A): Validated[E, B]

  def map[C](func: B => C): CheckTake1[E, A, C] = new CheckTake1[E, A, C] {
    override def f(a: A): Validated[E, C] =
      self.apply(a).map(func)
  }
}
