package scala_cats.chapter05.ex_5_4

import cats.implicits.catsSyntaxEitherId
import scala_cats.UnitSpec
import scala.concurrent.Await
import scala.concurrent.duration._
import Autobot._

class AutobotSpec extends UnitSpec {
  "autobot power level" can "return level if autobot found" in {
    assert(
      Await.result(getPowerLevel("Bumblebee").value, 1.second) == 8.asRight[String]
    )
    assert(
      Await.result(getPowerLevel2("Bumblebee").value, 1.second) == 8.asRight[String]
    )
  }

  it can "return error message if autobot not found" in {
    assert(
      Await.result(getPowerLevel("Nigel").value, 1.second) == "Could not reach Nigel".asLeft[Int]
    )
    assert(
      Await.result(getPowerLevel2("Nigel").value, 1.second) == "Could not reach Nigel".asLeft[Int]
    )
  }

  "autobot can special move" can "return true if combined power level is greater than 15" in {
    assert(
      Await.result(canSpecialMove("Bumblebee", "Hot Rod").value, 1.second) == true.asRight[String]
    )
  }

  it can "return false if combined power level is less than 15" in {
    assert(
      Await.result(canSpecialMove("Bumblebee", "Jazz").value, 1.second) == false.asRight[String]
    )
  }

  it can "return false if autobot not found" in {
    assert(
      Await.result(canSpecialMove("Bumblebee", "Jazzy").value, 1.second) ==
        "Could not reach Jazzy".asLeft[Int]
    )
  }

  "tactical report" can "report success if combined power level is greater than 15" in {
    assert(
      tacticalReport("Bumblebee", "Hot Rod") == "Bumblebee and Hot Rod are ready to roll out!"
    )
  }

  it can "report failure if combined power level is less than 15" in {
    assert(
      tacticalReport("Jazz","Bumblebee") == "Jazz and Bumblebee need a recharge."
    )
  }

  it can "report failure if autobot not found" in {
    assert(
      tacticalReport("Jazzy", "Bumblebee") == "Comms error: Could not reach Jazzy"
    )
  }
}