package scala_cats.chapter04

import cats.{ApplicativeError, MonadError}

import scala.util.Try

//noinspection TypeAnnotation
object MyMonadError {
  type ErrorOr[A] = Either[String, A]
  type ExceptionOr[A] = Either[Throwable, A]

  val catsMonadError: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]
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

  implicit val meTry = MonadError[Try, Throwable]
  implicit val meExceptionOr = MonadError[ExceptionOr, Throwable]

  // Example taken from https://typelevel.org/cats/typeclasses/applicativemonaderror.html
  def getCityClosestToCoordinate[F[_]](x: (Int, Int))(implicit ae: ApplicativeError[F, String]): F[String] = {
    println(x)
    if (x == ((0, 0)))
      ae.raiseError("(0,0) is at the singularity, dummy!")
    else
      ae.pure("Minneapolis, MN")
  }

  def getTemperatureByCity[F[_]](city: String)(implicit ae: ApplicativeError[F, String]): F[Int] = {
    println(city)
    ae.pure(78)
  }

  // TODO - The *[_] does something special wrt implicits
  def getTemperatureByCoordinates[F[_] : MonadError[*[_], String]](x: (Int, Int)): F[Int] = {
    import cats.implicits._
    for {
      c <- getCityClosestToCoordinate[F](x)
      t <- getTemperatureByCity[F](c)
    } yield t
  }

  def getTemperatureByCoordinatesAlternate[F[_]](x: (Int, Int))(implicit me: MonadError[F, String]): F[Int] = {
    import cats.implicits._

    if (x._1 < 0 || x._2 < 0)
      me.raiseError("Invalid Coordinates")
    else {
      (for {
        c <- getCityClosestToCoordinate[F](x)
        t <- getTemperatureByCity[F](c)
      } yield t)
        .flatTap(i => {
          println(s"x: $x res: $i")
          me.pure(())
        })
        .attemptTap {
          case Left(err) =>
            println(s"Oh dear: $err")
            me.pure(())
          case Right(i) =>
            println(s"x: $x res: $i")
            me.pure(())
        }
    }
  }
}