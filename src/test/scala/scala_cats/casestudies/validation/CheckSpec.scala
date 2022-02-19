package scala_cats.casestudies.validation

import cats.Monoid
import scala_cats.UnitSpec
import cats.syntax.either._
import cats.syntax.parallel._
import cats.Semigroup
import cats.data.Validated.{Invalid, Valid}

class CheckSpec extends UnitSpec {
  private val validValue = "A valid value"
  private val failureMsg1 = "Something has gone wrong"
  private val failureMsg2 = "Spanner in the works!"
  private val largerThan2FailureMsg = "Must be > 2"
  private val largerThan4FailureMsg = "Must be > 4"

  // Need semigroup (to combine errors) and monoid (to combine values)
  def combineEithers[E: Semigroup, A: Monoid](v1: Either[E, A], v2: Either[E, A]): Either[E, A] = {
    (v1, v2).parMapN(Monoid[A].combine)
  }

  type ErrorOr[A] = Either[Vector[String], A]

  // Now only need monoid, as type of error is locked down
  def combineErrorOrs[A: Monoid](v1: ErrorOr[A], v2: ErrorOr[A]): ErrorOr[A] = {
    (v1, v2).parMapN(Monoid[A].combine)
  }

  "parMapN" can "combine eithers and retain error information" in {
    val yes = "Yes".asRight[String]
    val no = "No".asLeft[String]
    val ya = "Ya".asRight[String]
    val nein = "Nein".asLeft[String]

    assert((yes, no).parMapN(_ + _) == no)
    assert((yes, ya).parMapN(_ + _) == Right(s"YesYa"))
    assert((yes, nein).parMapN(_ + _) == nein)
    assert((no, ya).parMapN(_ + _) == no)
    assert((no, nein).parMapN(_ + _) == Left("NoNein"))
    assert((ya, nein).parMapN(_ + _) == nein)

    val yesEO: ErrorOr[String] = "Yes".asRight
    val noEO: ErrorOr[String] = Vector("No").asLeft
    val yaEO: ErrorOr[String] = "Ya".asRight
    val neinEO: ErrorOr[String] = Vector("Nein").asLeft[String]

    assert(combineErrorOrs(yesEO, noEO) == noEO)
    assert(combineErrorOrs(yesEO, yaEO) == Right("YesYa"))
    assert(combineErrorOrs(yesEO, neinEO) == neinEO)
    assert(combineErrorOrs(noEO, yaEO) == noEO)
    assert(combineErrorOrs(noEO, neinEO) == Left(Vector("No", "Nein")))
    assert(combineErrorOrs(yaEO, neinEO) == neinEO)
  }

  private val passingCheckable = new PassingCheck[Vector[String], String]
  private val failingCheckable1 = FailingCheck[Vector[String], String](Vector(failureMsg1))
  private val failingCheckable2 = FailingCheck[Vector[String], String](Vector(failureMsg2))

  "Checkable and method" can "return value if both checks pass" in {
    assert(passingCheckable.and(passingCheckable).apply(validValue) == Right(validValue))
  }

  it can "return single error if one check fails" in {
    assert(passingCheckable.and(failingCheckable1).apply(validValue) == Left(Vector(failureMsg1)))
    assert(failingCheckable2.and(passingCheckable).apply(validValue) == Left(Vector(failureMsg2)))
  }

  it can "return both errors if both checks fail" in {
    assert(failingCheckable1.and(failingCheckable2).apply(validValue) == Left(Vector(failureMsg1, failureMsg2)))
  }

  private val largerThan2CheckEither: Check_Either[List[String], Int] = Check_Either.Pure {
    v =>
      if (v > 2) v.asRight else {
        List(largerThan2FailureMsg).asLeft
      }
  }

  private val largerThan4CheckEither: Check_Either[List[String], Int] = Check_Either.Pure {
    v =>
      if (v > 4) v.asRight else {
        List(largerThan4FailureMsg).asLeft
      }
  }

  "Check_Either and method" can "return value if both checks pass" in {
    assert(largerThan2CheckEither.and(largerThan4CheckEither)(5) == Right(5))
  }

  it can "return single error if one check fails" in {
    assert(largerThan2CheckEither.and(largerThan4CheckEither)(3) == Left(List(largerThan4FailureMsg)))
  }

  it can "return both errors if both checks fail" in {
    assert(largerThan2CheckEither.and(largerThan4CheckEither)(1) == Left(List(largerThan2FailureMsg, largerThan4FailureMsg)))
  }

  private val passingCheckF = CheckF[Vector[String], String](a => a.asRight)
  private val failingCheckF1 = CheckF[Vector[String], String](_ => Vector(failureMsg1).asLeft)
  private val failingCheckF2 = CheckF[Vector[String], String](_ => Vector(failureMsg2).asLeft)

  "CheckF and method" can "return value if both checks pass" in {
    assert(passingCheckF.and(passingCheckF).apply(validValue) == Right(validValue))
  }

  it can "return single error if one check fails" in {
    assert(passingCheckF.and(failingCheckF1).apply(validValue) == Left(Vector(failureMsg1)))
    assert(failingCheckF2.and(passingCheckF).apply(validValue) == Left(Vector(failureMsg2)))
  }

  it can "return both errors if both checks fail" in {
    assert(failingCheckF1.and(failingCheckF2).apply(validValue) == Left(Vector(failureMsg1, failureMsg2)))
  }

  it can "return and of two more checks" in {
    val largerThan2Msg = "Must be > 2"
    val smallerThanMinus2Msg = "Must be < -2"

    val largerThan2:CheckF[List[String], Int] = CheckF {
      v => if (v > 2) v.asRight else {
        List(largerThan2Msg).asLeft
      }
    }

    val lessThanMinus2:CheckF[List[String], Int] = CheckF {
      v => if (v < -2) v.asRight else {
        List(smallerThanMinus2Msg).asLeft
      }
    }

    assert(largerThan2.and(lessThanMinus2).apply(5) == Left(List(smallerThanMinus2Msg)))
    assert(largerThan2.and(lessThanMinus2).apply(-3) == Left(List(largerThan2Msg)))
    assert(largerThan2.and(lessThanMinus2).apply(0) == Left(List(largerThan2Msg, smallerThanMinus2Msg)))
  }

  private val largerThan2CheckFGoldilocks:CheckF_Goldilocks[List[String], Int] = CheckF_Goldilocks {
    v => if (v > 2) v.asRight else {
      List(largerThan2FailureMsg).asLeft
    }
  }

  private val largerThan4CheckFGoldilocks:CheckF_Goldilocks[List[String], Int] = CheckF_Goldilocks {
    v => if (v > 4) v.asRight else {
      List(largerThan4FailureMsg).asLeft
    }
  }

  "CheckF_Goldilocks andOverpowered method" can "returns DOUBLE value if both checks pass" in {
    assert(largerThan2CheckFGoldilocks.andOverpowered(largerThan4CheckFGoldilocks).apply(5) == Right(10))
  }

  it can "return single error if one check fails" in {
    assert(largerThan2CheckFGoldilocks.and(largerThan4CheckFGoldilocks).apply(3) == Left(List(largerThan4FailureMsg)))
  }

  it can "return both errors if both checks fail" in {
    assert(largerThan2CheckFGoldilocks.and(largerThan4CheckFGoldilocks).apply(0) == Left(List(largerThan2FailureMsg, largerThan4FailureMsg)))
  }

  "CheckF_Goldilocks and method" can "returns value if both checks pass" in {
    assert(largerThan2CheckFGoldilocks.and(largerThan4CheckFGoldilocks).apply(5) == Right(5))
  }

  it can "return single error if one check fails" in {
    assert(largerThan2CheckFGoldilocks.and(largerThan4CheckFGoldilocks).apply(3) == Left(List(largerThan4FailureMsg)))
  }

  it can "return both errors if both checks fail" in {
    assert(largerThan2CheckFGoldilocks.and(largerThan4CheckFGoldilocks).apply(0) == Left(List(largerThan2FailureMsg, largerThan4FailureMsg)))
  }

  "CheckF_Goldilocks andUnderpowered method" can "returns value if both checks pass" in {
    assert(largerThan2CheckFGoldilocks.andUnderpowered(largerThan4CheckFGoldilocks).apply(5) == Right(5))
  }

  it can "return single error if one check fails" in {
    assert(largerThan2CheckFGoldilocks.andUnderpowered(largerThan4CheckFGoldilocks).apply(3) == Left(List(largerThan4FailureMsg)))
  }

  it can "return ONLY ONE ERROR if both checks fail" in {
    assert(largerThan2CheckFGoldilocks.andUnderpowered(largerThan4CheckFGoldilocks).apply(0) == Left(List(largerThan2FailureMsg)))
  }

  private val largerThan2CheckValidated: Check_Validated[List[String], Int] = Check_Validated.Pure {
    v =>
      if (v > 2) Valid(v) else {
        Invalid(List(largerThan2FailureMsg))
      }
  }

  private val largerThan4CheckValidated: Check_Validated[List[String], Int] = Check_Validated.Pure {
    v =>
      if (v > 4) Valid(v) else {
        Invalid(List(largerThan4FailureMsg))
      }
  }

  "Check_Validated and method" can "return value if both checks pass" in {
    assert(largerThan2CheckValidated.and(largerThan4CheckValidated)(5) == Valid(5))
  }

  it can "return single error if one check fails" in {
    assert(largerThan2CheckValidated.and(largerThan4CheckValidated)(3) == Invalid(List(largerThan4FailureMsg)))
  }

  it can "return both errors if both checks fail" in {
    assert(largerThan2CheckValidated.and(largerThan4CheckValidated)(1) == Invalid(List(largerThan2FailureMsg, largerThan4FailureMsg)))
  }

  "Check_Validated and method with applyOverpowered" can "returns DOUBLE value if both checks pass" in {
    assert(largerThan2CheckValidated.and(largerThan4CheckValidated).applyOverpowered(5) == Valid(10))
  }

  it can "return single error if one check fails" in {
    assert(largerThan2CheckValidated.and(largerThan4CheckValidated).applyOverpowered(3) == Invalid(List(largerThan4FailureMsg)))
  }

  it can "return both errors if both checks fail" in {
    assert(largerThan2CheckValidated.and(largerThan4CheckValidated).applyOverpowered(1) == Invalid(List(largerThan2FailureMsg, largerThan4FailureMsg)))
  }

  "Check_Validated or method" can "return value if both checks pass" in {
    assert(largerThan2CheckValidated.or(largerThan4CheckValidated)(5) == Valid(5))
  }

  it can "return value if one check passes" in {
    assert(largerThan2CheckValidated.or(largerThan4CheckValidated)(3) == Valid(3))
  }

  it can "return both errors if both checks fail" in {
    assert(largerThan2CheckValidated.or(largerThan4CheckValidated)(1) == Invalid(List(largerThan2FailureMsg, largerThan4FailureMsg)))
  }
}