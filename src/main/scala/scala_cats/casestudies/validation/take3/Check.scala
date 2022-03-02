package scala_cats.casestudies.validation.take3

import cats.Semigroup
import cats.data.Validated

sealed trait Check[E, A, B] {
  import Check._

  // this function needs to be implemented per trait
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

  final case class Pure[E, A](
                               f: A => Validated[E, A]
                             ) extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      f(a)
  }

  final case class PurePredicate[E, A](
                                        pred: Predicate[E, A]
                                      ) extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      pred(a)
  }

  def apply[E, A](f: A => Validated[E, A]): Check[E, A, A] = Pure(f)

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = PurePredicate(pred)
}