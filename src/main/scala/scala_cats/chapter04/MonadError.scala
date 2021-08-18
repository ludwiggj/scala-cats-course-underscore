package scala_cats.chapter04

import cats.{MonadError => CatsMonadError}

import scala.util.Try

//noinspection TypeAnnotation
object MonadError {
  type ErrorOr[A] = Either[String, A]
  type ExceptionOr[A] = Either[Throwable, A]

  val catsMonadError: CatsMonadError[ErrorOr, String] = CatsMonadError[ErrorOr, String]
  val exn: Throwable = new RuntimeException("It's all gone wrong")

  def handleErrorWith[A](failure: ErrorOr[A], okValue: A): ErrorOr[A] = {
    catsMonadError.handleErrorWith(failure) {
      case "Badness" =>
        catsMonadError.pure(okValue)

      case _ =>
        catsMonadError.raiseError("It's not ok")
    }
  }

  def handleError[A](failure: ErrorOr[A], okValue: A, notOkValue: A): ErrorOr[A] = {
    catsMonadError.handleError(failure) {
      case "Badness" => okValue

      case _ => notOkValue
    }
  }

  implicit val meTry = CatsMonadError[Try, Throwable]
  implicit val meExceptionOr = CatsMonadError[ExceptionOr, Throwable]
}
