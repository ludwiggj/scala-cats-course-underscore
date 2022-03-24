package scala_cats.casestudies.validation.take4

import cats.syntax.either._
import scala_cats.UnitSpec

class CheckSpec extends UnitSpec {
  import Checks._
  import Errors._

  "username check" can "return true if user name meets all criteria" in {
    assert(isValidUserNameCheck.run("ludwiggj") == "ludwiggj".asRight[Errors])
  }

  it can "return error if user name is shorter than 4 characters" in {
    assert(isValidUserNameCheck.run("lud") == mustBeLongerThan(3).asLeft[String])
  }

  it can "return error if user name is not alphanumeric" in {
    assert(isValidUserNameCheck.run("ludwigg-") == mustBeAlphaNumeric.asLeft[String])
  }

  it can "return errors if user name is shorter than 4 characters and is not alphanumeric" in {
    assert(isValidUserNameCheck.run("**") == (mustBeLongerThan(3) concatNel mustBeAlphaNumeric).asLeft[String])
  }

  // An email address must contain an @ sign. Split the string at the @. The
  // string to the left must not be empty. The string to the right must be at
  // least three characters long and contain a dot.
  "textbook email address check" can "return error if it does not contain an @" in {
    assert(emailCheck("ludwig") == mustContainCharacterOnce('@').asLeft[String])
  }

  it can "return error if @ is first character" in {
    assert(emailCheck("@itv.com") == mustBeLongerThan(0, Some("email_address_before_@")).asLeft[String])
  }

  it can "return error if second part of email address is too short" in {
    assert(emailCheck("g@i.") == mustBeLongerThan(2, Some("email_address_after_@")).asLeft[String])
  }

  it can "return error if second part of email address does not contain a ." in {
    assert(emailCheck("g@itvcom") == mustContainCharacter('.', Some("email_address_after_@")).asLeft[String])
  }

  it can "return errors if @ is first character and second part of email address is too short" in {
    assert(emailCheck("@i.") ==
      (
        mustBeLongerThan(0, Some("email_address_before_@")) concatNel
          mustBeLongerThan(2, Some("email_address_after_@"))
        ).asLeft[String]
    )
  }

  it can "return errors if @ is first character and second part of email address is too short and does not contain a ." in {
    assert(emailCheck("@i") ==
      (
        mustBeLongerThan(0, Some("email_address_before_@")) concatNel
          mustBeLongerThan(2, Some("email_address_after_@")) concatNel
          mustContainCharacter('.', Some("email_address_after_@"))
        ).asLeft[String]
    )
  }

  it can "return true if email address meets all criteria" in {
    assert(emailCheck("graeme.ludwig@itv.com") == "graeme.ludwig@itv.com".asRight[Errors])
  }
}