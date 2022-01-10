package scala_cats.chapter04.ex_4_8_3

import cats.data.Reader
import cats.implicits.catsSyntaxApplicativeId

final case class Db(usernames: Map[Int, String],
                    passwords: Map[String, String])

object Db {
  type DbReader[A] = Reader[Db, A]

  private val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  private val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      maybeUsername <- findUsername(userId)
      passwordMatches <- if (maybeUsername.isEmpty)
        Reader[Db, Boolean](_ => false)
      else
        checkPassword(maybeUsername.get, password)
    } yield passwordMatches

  def checkLoginTextbook(userId: Int, password: String): DbReader[Boolean] =
    for {
      maybeUsername <- findUsername(userId)
      passwordMatches <- maybeUsername.map { username =>
        checkPassword(username, password)
      }.getOrElse {
        false.pure[DbReader]
      }
    } yield passwordMatches
}
