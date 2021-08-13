package scala_cats.chapter04.ex_4_5_4

import cats.MonadError

object Validate {

  def illegalAgeException(age: Int): IllegalArgumentException = {
    new IllegalArgumentException(s"Age must be greater than or equal to $age")
  }

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    me.ensure(me.pure(age))(illegalAgeException(age))(_ >= 18)
}