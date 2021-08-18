package scala_cats.chapter04

import cats.Monad

object CatsMonad {

  import cats.syntax.flatMap._
  import cats.syntax.functor._ // for flatMap

  def sumSquareDesugared[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = {
    a.flatMap(x => b.map(y => x * x + y * y))
  }

  def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = {
    for {
      x <- a
      y <- b
    } yield x * x + y * y
  }
}
