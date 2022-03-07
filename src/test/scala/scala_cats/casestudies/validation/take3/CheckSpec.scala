package scala_cats.casestudies.validation.take3

import cats.data.NonEmptyList
import cats.syntax.validated._
import scala_cats.UnitSpec

class CheckSpec extends UnitSpec {
  import Errors._
  import Checks._

  def errors(first: String, rest: List[String]): Errors = NonEmptyList(first, rest)

  "username check" can "return true if user name meets all criteria" in {
    assert(isValidUserNameCheck("ludwiggj") == "ludwiggj".valid)
  }

  it can "return error if user name is shorter than 4 characters" in {
    assert(isValidUserNameCheck("lud") == mustBeLongerThan(3).invalid)
  }

  it can "return error if user name is not alphanumeric" in {
    assert(isValidUserNameCheck("ludwigg-") == mustBeAlphaNumeric.invalid)
  }

  it can "return errors if user name is shorter than 4 characters and is not alphanumeric" in {
    assert(isValidUserNameCheck("**") == (mustBeLongerThan(3) concatNel mustBeAlphaNumeric).invalid)
  }

  // An email address must contain an @ sign. Split the string at the @. The
  // string to the left must not be empty. The string to the right must be at
  // least three characters long and contain a dot.
  "email address check" can "return error if it does not contain an @" in {
    assert(isValidEmailAddressCheck("ludwig") == mustContainCharacterOnce('@').invalid)
  }

  it can "return error if @ is first character" in {
    assert(isValidEmailAddressCheck("@itv.com") == mustBeLongerThan(0, Some("email_address_before_@")).invalid)
  }

  it can "return error if second part of email address is too short" in {
    assert(isValidEmailAddressCheck("g@i.") == mustBeLongerThan(2, Some("email_address_after_@")).invalid)
  }

  it can "return error if second part of email address does not contain a ." in {
    assert(isValidEmailAddressCheck("g@itvcom") == mustContainCharacter('.', Some("email_address_after_@")).invalid)
  }

  it can "return errors if @ is first character and second part of email address is too short" in {
    assert(isValidEmailAddressCheck("@i.") ==
      (
        mustBeLongerThan(0, Some("email_address_before_@")) concatNel
          mustBeLongerThan(2, Some("email_address_after_@"))
      ).invalid
    )
  }

  it can "return errors if @ is first character and second part of email address is too short and does not contain a ." in {
    assert(isValidEmailAddressCheck("@i") ==
      (
        mustBeLongerThan(0, Some("email_address_before_@")) concatNel
          mustBeLongerThan(2, Some("email_address_after_@")) concatNel
          mustContainCharacter('.', Some("email_address_after_@"))
      ).invalid
    )
  }

  it can "return true if email address meets all criteria" in {
    assert(isValidEmailAddressCheck("graeme.ludwig@itv.com") == "graeme.ludwig@itv.com".valid)
  }

  "textbook email address check" can "return error if it does not contain an @" in {
    assert(emailCheck("ludwig") == mustContainCharacterOnce('@').invalid)
  }

  it can "return error if @ is first character" in {
    assert(emailCheck("@itv.com") == mustBeLongerThan(0, Some("email_address_before_@")).invalid)
  }

  it can "return error if second part of email address is too short" in {
    assert(emailCheck("g@i.") == mustBeLongerThan(2, Some("email_address_after_@")).invalid)
  }

  it can "return error if second part of email address does not contain a ." in {
    assert(emailCheck("g@itvcom") == mustContainCharacter('.', Some("email_address_after_@")).invalid)
  }

  it can "return errors if @ is first character and second part of email address is too short" in {
    assert(emailCheck("@i.") ==
      (
        mustBeLongerThan(0, Some("email_address_before_@")) concatNel
          mustBeLongerThan(2, Some("email_address_after_@"))
        ).invalid
    )
  }

  it can "return errors if @ is first character and second part of email address is too short and does not contain a ." in {
    assert(emailCheck("@i") ==
      (
        mustBeLongerThan(0, Some("email_address_before_@")) concatNel
          mustBeLongerThan(2, Some("email_address_after_@")) concatNel
          mustContainCharacter('.', Some("email_address_after_@"))
        ).invalid
    )
  }

  it can "return true if email address meets all criteria" in {
    assert(emailCheck("graeme.ludwig@itv.com") == "graeme.ludwig@itv.com".valid)
  }
}