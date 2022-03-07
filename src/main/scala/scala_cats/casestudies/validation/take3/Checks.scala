package scala_cats.casestudies.validation.take3

import scala_cats.casestudies.validation.take3.Errors.Errors
import scala_cats.casestudies.validation.take3.Predicates._
import cats.syntax.validated._
import cats.syntax.apply._     // for MapN
import cats.syntax.semigroup._ // for |+|

object Checks {
  // A username must contain at least four characters and consist entirely
  // of alphanumeric characters
  def isValidUserNameCheck: Check[Errors, String, String] =
    Check(longerThan(3).and(alphaNumeric))

  def isValidEmailAddress: Predicate[Errors, Array[String]] = {
    longerThan[Array[String]](0, inputArray => inputArray(0), Some("email_address_before_@")).and(
      longerThan[Array[String]](2, inputArray => inputArray(1), Some("email_address_after_@")).and(
        contains[Array[String]]('.', inputArray => inputArray(1), Some("email_address_after_@"))
      )
    )
  }

  // An email address must contain an @ sign. Split the string at the @. The
  // string to the left must not be empty. The string to the right must be at
  // least three characters long and contain a dot.
  def isValidEmailAddressCheck: Check[Errors, String, String] =
    Check(containsOnce('@'))
      .map(_.split('@'))
      .andThen(Check(isValidEmailAddress))
      .map(_.mkString("@"))

  // Textbook answer

  // An email address must contain a single `@` sign.
  // Split the string at the `@`.
  // The string to the left must not be empty.
  // The string to the right must be
  // at least three characters long and contain a dot.

  // This method uses Check.apply[E, A, B](f: A => Validated[E, B]): Check[E, A, B]
  // It splits email address at @
  val splitEmail: Check[Errors, String, (String, String)] =
    Check(_.split('@') match {
      case Array(name, domain) =>
        (name, domain).validNel[String] // Actual:
                                        // data.ValidatedNel[String, (String, String)]
                                        // type ValidatedNel[+E, +A] = Validated[NonEmptyList[E], A]
                                        // => Validated[NonEmptyList[String], (String, String)]

                                        // Expected:
                                        // Validated[Errors, (String, String)]
                                        // type Errors = NonEmptyList[String]
                                        // => Validated[NonEmptyList[String], (String, String)]

      case _ => "Must contain the character @ exactly once".invalidNel[(String, String)]
    })

  // This is repeating code, as the input is (String, String) rather than String
  val checkLeftNotTextbook: Check[Errors, (String, String), (String, String)] =
    Check { input => input match {
      case (left, _) => if (left.nonEmpty) input.validNel[String] else "err".invalidNel[(String, String)]
    }}

  // This solves the issue
  val checkLeft: Check[Errors, String, String] =
    Check(longerThan[String](0, identity, Some("email_address_before_@")))

  // This will not return all of the errors
  val checkRightNotTextbook: Check[Errors, String, String] =
    Check(longerThan[String](2, identity, Some("email_address_after_@"))) andThen
      Check(contains[String]('.', identity, Some("email_address_after_@")))

  // This will return all of the errors
  val checkRight: Check[Errors, String, String] =
    Check(
      longerThan[String](2, identity, Some("email_address_after_@"))
        and contains[String]('.', identity, Some("email_address_after_@"))
    )

  // Signature inverts that of splitEmail
  val joinEmail: Check[Errors, (String, String), String] =
    Check { input => input match {
      case (left, right) => (checkLeft(left), checkRight(right)).mapN {
        case (l, r) => l |+| "@" |+| r
      }
    }}

  def emailCheck: Check[Errors, String, String] = splitEmail andThen joinEmail
}
