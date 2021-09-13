package scala_cats.chapter04

import cats.{ApplicativeError => CatsApplicativeError}

// From cats documentation (https://typelevel.org/cats/typeclasses/applicativemonaderror.html)
object ApplicativeError {
  val divisorIs0 = "divisor is zero"
  val badMath = "Bad Math"
  val wasteOfTime = "Waste of Time"

  def attemptDivide(x: Int, y: Int): Either[String, Int] = {
    if (y == 0) {
      Left(divisorIs0)
    }
    else {
      Right(x / y)
    }
  }

  // Let's abstract across container types
  def attemptDivideApplicativeError[F[_]](x: Int, y: Int)(implicit ae: CatsApplicativeError[F, String]): F[Int] = {
    if (y == 0)
      ae.raiseError(divisorIs0)
    else
      ae.pure(x / y)
  }

  def attemptDivideApplicativeErrorWithMap2[F[_]](x: Int, y: Int)(implicit ae: CatsApplicativeError[F, String]): F[Int] = {
    if (y == 0)
      ae.raiseError(divisorIs0)
    else {
      val fx = ae.pure(x)
      val fy = ae.pure(y)
      ae.map2(fx, fy)(_ / _)
    }
  }

  def attemptDivideApplicativeErrorAbove2[F[_]](x: Int, y: Int)(implicit ae: CatsApplicativeError[F, String]): F[Int] = {
    if (y == 0)
      ae.raiseError(badMath)
    else if (y == 1) {
      ae.raiseError(wasteOfTime)
    }
    else {
      val fx = ae.pure(x)
      val fy = ae.pure(y)
      ae.map2(fx, fy)(_ / _)
    }
  }

  def handle[F[_]](f: F[Int])(implicit ae: CatsApplicativeError[F, String]): F[Int] = {
    ae.handleError(f) {
      case "Bad Math" => -1
      case "Waste of Time" => -2
      case _ => -3
    }
  }

  def handleAsEither[F[_]](f: F[Int])(implicit ae: CatsApplicativeError[F, String]): F[Either[String, Int]] = {
    ae.attempt(f)
  }

  object WithThrowable {
    val badMathException = new IllegalArgumentException("0 - bad math")
    val wasteOfTimeException = new IllegalArgumentException("1 - waste of time")

    def attemptDivideApplicativeErrorAbove2[F[_]](x: Int, y: Int)(implicit ae: CatsApplicativeError[F, Throwable]): F[Either[Throwable, Int]] = {
      ae.attempt {
        if (y == 0)
          ae.raiseError(badMathException)
        else if (y == 1) {
          ae.raiseError(wasteOfTimeException)
        }
        else {
          val fx = ae.pure(x)
          val fy = ae.pure(y)
          ae.map2(fx, fy)(_ / _)
        }
      }
    }
  }
}