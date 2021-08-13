package scala_cats.chapter04

import cats.implicits.catsSyntaxEitherId
import scala_cats.chapter04.wrapper.{LoginError, PasswordIncorrect, UnexpectedError, UserNotFound}

case class User(username: String, password: String)

object User {
  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit = {
    error match {
      case UserNotFound(username) =>
        println(s"User not found: $username")

      case PasswordIncorrect(username) =>
        println(s"Password incorrect: $username")

      case UnexpectedError =>
      println("Unexpected error")
    }
  }

  def main(args: Array[String]): Unit = {
    val result1: LoginResult = User("dave", "passw0rd").asRight //[LoginError]
    val result2: LoginResult = UserNotFound("dave").asLeft //[User]

    result1.fold(handleError, println)
    result2.fold(handleError, println)
  }
}