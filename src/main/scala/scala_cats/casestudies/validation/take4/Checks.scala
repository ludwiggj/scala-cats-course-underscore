package scala_cats.casestudies.validation.take4

import cats.data.Kleisli
import cats.instances.either._
import cats.syntax.parallel._  // for parMapN
import cats.syntax.semigroup._ // for |+|
import Errors.Errors
import Predicates._

object Checks {
  // Type inference can be tricky in this exercise. We found that the following definitions helped us
  // to write code with fewer type declarations.
  type Result[A] = Either[Errors, A]

  type Check[A, B] = Kleisli[Result, A, B]

  // Create a check from a function:
  def check[A, B](func: A => Result[B]): Check[A, B] =
    Kleisli(func)

  // Create a check from a Predicate:
  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
    Kleisli[Result, A, A](pred.run)

  // A username must contain at least four characters and consist entirely
  // of alphanumeric characters
  def isValidUserNameCheck: Check[String, String] =
    checkPred(longerThan(3).and(alphaNumeric))

  // Textbook answer

  // An email address must contain a single `@` sign.
  // Split the string at the `@`.
  // The string to the left must not be empty.
  // The string to the right must be
  // at least three characters long and contain a dot.

  // This method uses Check.apply[E, A, B](f: A => Validated[E, B]): Check[E, A, B]
  // It splits email address at @
  val splitEmail: Check[String, (String, String)] = {
    check(x => {
      x.split('@') match {
        case Array(name, domain) =>
          Right((name, domain))

        case _ =>
          Left(Errors.error("Must contain the character @ exactly once"))
      }}
    )
  }

  val checkLeft: Check[String, String] =
    checkPred(longerThan[String](0, identity, Some("email_address_before_@")))

  val checkRight: Check[String, String] =
    checkPred(
      longerThan[String](2, identity, Some("email_address_after_@"))
        and contains[String]('.', identity, Some("email_address_after_@"))
    )

  // Signature inverts that of splitEmail
  val joinEmail: Check[(String, String), String] =
    check {
      case (left, right) =>
        (checkLeft(left), checkRight(right)).parMapN(_ |+| "@" |+| _)
    }

  def emailCheck: Check[String, String] = splitEmail andThen joinEmail
}