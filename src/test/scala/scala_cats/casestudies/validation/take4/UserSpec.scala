package scala_cats.casestudies.validation.take4

import cats.syntax.either._
import scala_cats.UnitSpec
import scala_cats.casestudies.validation.take4.Errors.Errors

class UserSpec extends UnitSpec {

  "createUser" can "create a valid user" in {
    val username = "ludwiggj"
    val email = "graeme.ludwig@somewhere.com"

    assert(User.createUser(username, email) == User(username, email).asRight[Errors])
  }

  it can "flag all errors" in {
    val username = "lud"
    val email = "@i"

    val expected = Errors.mustBeLongerThan(3).concatNel(
      Errors.mustBeLongerThan(0, Some("email_address_before_@")).concatNel(
        Errors.mustBeLongerThan(2, Some("email_address_after_@")).concatNel(
          Errors.mustContainCharacter('.', Some("email_address_after_@"))
        )
      )
    ).asLeft[User]

    assert(User.createUser(username, email) == expected)
  }
}