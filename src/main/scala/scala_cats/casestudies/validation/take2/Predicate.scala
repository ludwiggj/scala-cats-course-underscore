package scala_cats.casestudies.validation.take2

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.Semigroup
import cats.syntax.apply._     // for MapN
import cats.syntax.semigroup._ // for |+|
import Predicate._

// This is basically Check_Validated from take 1, renamed as Predicate
sealed trait Predicate[E, A] {
  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(func) => func(a)

    case And(left, right) =>
      (left(a), right(a)).mapN((_, _) => a)

    case Or(left, right) =>
      (left(a), right(a)) match {
        case (Valid(a), Valid(_)) => Valid(a)
        case (Invalid(_), Valid(a)) => Valid(a)
        case (Valid(a), Invalid(_)) => Valid(a)
        case (Invalid(e1), Invalid(e2)) => Invalid(e1 |+| e2)
     }
  }
}

object Predicate {
  final case class And[E, A](
                              left: Predicate[E, A],
                              right: Predicate[E, A]
                            ) extends Predicate[E, A]

  final case class Or[E, A](
                             left: Predicate[E, A],
                             right: Predicate[E, A]
                           ) extends Predicate[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

  def pure[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)
}