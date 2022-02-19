package scala_cats.casestudies.validation

import cats.Semigroup
import cats.syntax.parallel._ // for parMapN
import cats.syntax.apply._    // for mapN

final case class CheckF_Goldilocks[E, A](func: A => Either[E, A]) {
  def apply(value: A): Either[E, A] = func(value)

  // NOTE: This will combine a valid value [A] twice, via the Semigroup[A] e.g. 5 becomes 10, yes becomes yesyes
  //       This handles errors correctly because parMapN implementation for Either accumulates errors
  //       Semigroup[E] combines the error type values
  def andOverpowered(that: CheckF_Goldilocks[E, A])(implicit sa: Semigroup[A],
                                                    se: Semigroup[E]): CheckF_Goldilocks[E, A] = CheckF_Goldilocks { a =>
    (apply(a), that(a)).parMapN(Semigroup[A].combine)
  }

  // NOTE: Simplification of andOverpowered to give correct behaviour for valid values.
  //       Semigroup[A] no longer needed
  def and(that: CheckF_Goldilocks[E, A])(implicit s: Semigroup[E]): CheckF_Goldilocks[E, A] = CheckF_Goldilocks { a =>
    (apply(a), that(a)).parMapN((_, _) => a)
  }

  // Switched from parMapN to mapN. Had to remove (implicit s: Semigroup[E]) from signature, as it's not used
  // This is because mapN implementation for Either fails fast on errors, which means that not all errors are captured
  def andUnderpowered(that: CheckF_Goldilocks[E, A]): CheckF_Goldilocks[E, A] = CheckF_Goldilocks { a =>
    (apply(a), that(a)).mapN((_, _) => a)
  }
}