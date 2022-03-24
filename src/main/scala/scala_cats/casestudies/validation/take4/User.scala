package scala_cats.casestudies.validation.take4

import cats.syntax.parallel._ // for parMapN

case class User(username: String, email: String)

object User {
  import Checks._

  def createUser(username: String, email: String): Result[User] = {
    (isValidUserNameCheck(username), emailCheck(email)).parMapN(User.apply)
  }
}