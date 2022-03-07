package scala_cats.casestudies.validation.take3

import cats.data.NonEmptyList

object Errors {
  type Errors = NonEmptyList[String]

  def error(s: String): Errors = NonEmptyList(s, Nil)

  def mustBeLongerThan(n: Int, fieldName: Option[String] = None): Errors =
    error(s"${fieldName.map(f => s"Field $f m").getOrElse("M")}ust be longer than $n characters")

  def mustContainCharacter(char: Char, fieldName: Option[String] = None): Errors =
    error(s"${fieldName.map(f => s"Field $f m").getOrElse("M")}ust contain character $char")

  val mustBeAlphaNumeric: Errors = error(s"Must be all alphanumeric characters")

  def mustContainCharacterOnce(char: Char): Errors = error(s"Must contain the character $char exactly once")
}