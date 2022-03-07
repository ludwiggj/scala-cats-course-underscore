package scala_cats.casestudies.validation.take3

import cats.Semigroup
import cats.data.Validated

// From 10.4.3 recap, a complete definition for each ADT
sealed trait Check[E, A, B] {
  import Check._

  // this function needs to be implemented per trait
  // Due to a type inference bug in Scala’s pattern matching, we’ve
  // switched to implementing apply using inheritance
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] = Map(this, f)

  def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] = FlatMap(this, f)

  def andThen[C](next: Check[E, B, C]): Check[E, A, C] = AndThen(this, next)
}

object Check {
  final case class Map[E, A, B, C](
                                    check: Check[E, A, B],
                                    f: B => C
                                  ) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = check(a) map f
  }

  final case class FlatMap[E, A, B, C](
                                        check: Check[E, A, B],
                                        f: B => Check[E, A, C]
                                      ) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => f(b)(a).toEither))
  }

  final case class AndThen[E, A, B, C](
                                        check: Check[E, A, B],
                                        next: Check[E, B, C]
                                      ) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => next(b).toEither))
  }

  // This is equivalent of Check.apply from take2
  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = PurePredicate(pred)

  final case class PurePredicate[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      pred(a)
  }

  // This is similar to PurePredicate, but has a more general type signature
  // It's key to the implementation of the email validation checks
  def apply[E, A, B](f: A => Validated[E, B]): Check[E, A, B] = Pure(f)

  final case class Pure[E, A, B](f: A => Validated[E, B]) extends Check[E, A, B] {
    def apply(a: A)(implicit s: Semigroup[E]):  Validated[E, B] =
      f(a)
  }
}