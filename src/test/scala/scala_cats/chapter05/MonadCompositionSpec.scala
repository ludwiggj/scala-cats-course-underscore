package scala_cats.chapter05

import cats.data.OptionT
import scala_cats.UnitSpec
import scala_cats.chapter05.MonadComposition.UserExample._

class MonadCompositionSpec extends UnitSpec {
  "look up user name" can "return user if found" in {
    assert(lookupUserName(1) == Right(Some("Bob")))
  }

  it can "return error if not found" in {
    assert(lookupUserName(-1).isLeft)

    assert(lookupUserName(-1) match {
      case Right(_) => false
      case Left(e: Error) => e.getMessage == "Cannot find user with id -1"
    })

    assert(lookupUserName(-1).left.toSeq.head.getMessage == "Cannot find user with id -1")

    assert(lookupUserName(-1).left.toOption.get.getMessage == "Cannot find user with id -1")
  }

  "look up user name 2" can "return user if found" in {
    val expected: OptionT[ErrorOr, String] = OptionT.some("Bob")
    assert(lookupUserName2(1) == expected)
  }

  it can "return error if not found" in {
    val none: OptionT[ErrorOr, String] = OptionT.none
    assert(lookupUserName2(-1) == none)
  }
}