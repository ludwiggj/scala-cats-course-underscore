package scala_cats.chapter04

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._ // for map
import cats.syntax.monad._

object CustomMonad {

  def retry[F[_] : Monad, A](start: A)(f: A => F[A]): F[A] =
    f(start).flatMap { a =>
      retry(a)(f)
    }

  def retryTailRecM[F[_] : Monad, A](start: A)(f: A => F[A]): F[A] =
    Monad[F].tailRecM(start) { a =>
      f(a).map(a2 => Left(a2))
    }

  def retryM[F[_] : Monad, A](start: A)(f: A => F[A]): F[A] =
    start.iterateWhileM(f)(_ => true)
}