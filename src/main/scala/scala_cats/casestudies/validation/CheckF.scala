package scala_cats.casestudies.validation

import cats.Semigroup
import cats.syntax.semigroup._ // for |+|

final case class CheckF[E, A](func: A => Either[E, A]) {
  def apply(value: A): Either[E, A] = func(value)

  def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] = CheckF { a =>
    (apply(a), that(a)) match {
      case (Right(a), Right(_)) => Right(a)
      case (Left(e), Right(_)) => Left(e)
      case (Right(_), Left(e)) => Left(e)
      case (Left(e1), Left(e2)) => Left(e1 |+| e2)
    }
  }
}