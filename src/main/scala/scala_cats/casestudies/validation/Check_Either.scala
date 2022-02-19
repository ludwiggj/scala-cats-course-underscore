package scala_cats.casestudies.validation

import cats.Semigroup
import cats.syntax.semigroup._

sealed trait Check_Either[E, A] {
  import Check_Either._

  def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] = this match {
    case Pure(func) => func(a)

    case And(left, right) =>
      // Either fails fast wrt errors, so we have to handle them explicitly
      // Validated accumulates the errors rather than fail fast; it's a better fit here
      // See MyValidated for a better implementation, based on Validated instead of Either
      (left(a), right(a)) match {
        case (Right(a), Right(_)) => Right(a)
        case (Left(e), Right(_)) => Left(e)
        case (Right(_), Left(e)) => Left(e)
        case (Left(e1), Left(e2)) => Left(e1 |+| e2)
    }
  }

  def and(that: Check_Either[E, A]): Check_Either[E, A] = And(this, that)
}

object Check_Either {
  final case class Pure[E, A](func: A => Either[E, A]) extends Check_Either[E, A]

  def pure[E, A](f: A => Either[E, A]): Check_Either[E, A] = Pure(f)

  final case class And[E, A](
                              left: Check_Either[E, A],
                              right: Check_Either[E, A]
                            ) extends Check_Either[E, A]
}