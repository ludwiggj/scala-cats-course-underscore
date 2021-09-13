package scala_cats.chapter04.ex_4_5_4

import cats.Eq
import cats.implicits.catsSyntaxEitherId
import scala_cats.UnitSpec
import scala_cats.chapter04.MyMonadError
import Validate.{illegalAgeException, validateAdult}

import scala.util.{Failure, Success, Try}

class ValidateSpec extends UnitSpec {
  "validateAge success" can "return a try" in {
    import scala_cats.chapter04.MyMonadError.meTry
    assert(validateAdult(21) == Success(21))
    assert(validateAdult[Try](21) == Success(21))
  }

  it can "return an either" in {
    import scala_cats.chapter04.MyMonadError.meExceptionOr
    assert(validateAdult(21) == Right(21))
  }

  "validateAge failure" can "return a try" in {
    import scala_cats.chapter04.MyMonadError.meTry

    // Following implicit combines with cats.Eq.catsStdEqForTry to provide
    // an eq method for Try[Int]
    implicit val eqT: Eq[Throwable] = Eq.allEqual

    assert(validateAdult(8) === Failure[Int](illegalAgeException(8)))
  }

  it can "return an either" in {
    import MyMonadError.meExceptionOr

    implicit val eqT: Eq[Throwable] = Eq.allEqual

    assert(validateAdult(8) === illegalAgeException(8).asLeft)
  }
}