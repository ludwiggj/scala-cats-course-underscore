package scala_cats.casestudies.validation.take1

import cats.Semigroup
import cats.syntax.either._    // for asRight
import cats.syntax.semigroup._ // for |+|

// check is A => Either[E, A]
trait Checkable[E, A] {
  def apply(value: A): Either[E, A]

  def and(that: Checkable[E, A])(implicit s: Semigroup[E]): Checkable[E, A] = (value: A) => {
    (apply(value), that(value)) match {
      case (Right(a), Right(_)) => Right(a)
      case (Left(e), Right(_)) => Left(e)
      case (Right(_), Left(e)) => Left(e)
      case (Left(e1), Left(e2)) => Left(e1 |+| e2)
    }
  }
}

class PassingCheck[E, A] extends Checkable[E, A] {
  override def apply(value: A): Either[E, A] = value.asRight[E]
}

case class FailingCheck[E, A](e: E) extends Checkable[E, A] {
  override def apply(value: A): Either[E, A] = e.asLeft[A]
}