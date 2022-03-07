package scala_cats.casestudies.validation.take1

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.apply._     // for mapN
import cats.syntax.semigroup._ // for |+|

sealed trait Check_Validated[E, A] {
  import Check_Validated._

  def applyOverpowered(a: A)(implicit se: Semigroup[E], sa: Semigroup[A]): Validated[E, A] = this match {
    case Pure(func) => func(a)

    case And(left, right) =>
      // This combines the valid values using semigroup, which isn't what we want
      // It also needs extra implicit parameter of type Semigroup[A]
      left(a).combine(right(a))

    case Or(_, _) => throw new NotImplementedError("Not implemented")
  }

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(func) => func(a)

    // Naive implementation of And
    // case And(left, right) =>
      // (left(a), right(a)) match {
        // case (Valid(a), Valid(_)) => Valid(a)
        // case (l, r) => l.combine(r)
      // }

    // Better implementation
    case And(left, right) =>
      (left(a), right(a)).mapN((_, _) => a)

    case Or(left, right) =>
      (left(a), right(a)) match {
        case (Valid(a), _) => Valid(a)
        case (_, Valid(a)) => Valid(a)
        case (Invalid(e1), Invalid(e2)) => Invalid(e1 |+| e2)
     }
  }

  def and(that: Check_Validated[E, A]): Check_Validated[E, A] = And(this, that)

  def or(that: Check_Validated[E, A]): Check_Validated[E, A] = Or(this, that)
}

object Check_Validated {
  final case class Pure[E, A](func: A => Validated[E, A]) extends Check_Validated[E, A]

  def pure[E, A](f: A => Validated[E, A]): Check_Validated[E, A] = Pure(f)

  final case class And[E, A](
                              left: Check_Validated[E, A],
                              right: Check_Validated[E, A]
                            ) extends Check_Validated[E, A]

  final case class Or[E, A](
                              left: Check_Validated[E, A],
                              right: Check_Validated[E, A]
                            ) extends Check_Validated[E, A]
}