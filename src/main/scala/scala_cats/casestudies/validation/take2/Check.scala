package scala_cats.casestudies.validation.take2

import cats.data.Validated
import cats.Semigroup

// My version
sealed trait CheckTake1[E, A, B] { self =>
  def apply(a: A): Validated[E, B] = f(a)

  // Added this function as it was the only way I saw to introduce type B for apply method
  // Actual answer is to not implement the apply method at all! (see answer below)
  def f(a: A): Validated[E, B]

  // This implementation is actually the right idea, but actual solution below uses case classes
  def map[C](func: B => C): CheckTake1[E, A, C] = new CheckTake1[E, A, C] {
    override def f(a: A): Validated[E, C] =
      self.apply(a).map(func)
  }
}

sealed trait Check[E, A, B] {
  import Check._

  // Actual answer was to not implement apply, but implement other methods (e.g. map) in terms of it
  // this function needs to be implemented per trait
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](func: B => C): Check[E, A, C] = Map(this, func)

  def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] = FlatMap(this, func)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen(this, that)
}

object Check {
  final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = check(a).map(func)
  }

  final case class FlatMap[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = {
      // NOTE: Validated does not have a flatMap method
      check(a).withEither(_.flatMap(func(_)(a).toEither))
    }
  }

  final case class AndThen[E, A, B, C](check1: Check[E, A, B], check2: Check[E, B, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check1(a).withEither(_.flatMap(b => check2(b).toEither))
  }

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = Pure(pred)

  final case class Pure[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = pred(a)
  }
}