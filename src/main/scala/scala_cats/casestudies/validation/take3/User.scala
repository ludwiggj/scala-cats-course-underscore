package scala_cats.casestudies.validation.take3

import cats.data.Validated
import cats.syntax.apply._ // for MapN
import scala_cats.casestudies.validation.take3.Errors.Errors

case class User(username: String, email: String)

object User {
  import Checks._

  def createUser(username: String, email: String): Validated[Errors, User] = {
    (isValidUserNameCheck(username), emailCheck(email)).mapN(User.apply)
  }
}
