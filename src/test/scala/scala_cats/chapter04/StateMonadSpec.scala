package scala_cats.chapter04

import cats.data.State
import scala_cats.UnitSpec
import scala_cats.chapter04.StateMonad._

class StateMonadSpec extends UnitSpec {

  "run" can "return state and value" in {
    assert(s.run(10).value == ((10, "The state is 10")))
  }

  "runS" can "return the state" in {
    assert(s.runS(10).value == 10)
  }

  "runA" can "return the value" in {
    assert(s.runA(10).value == "The state is 10")
  }

  "flatMap" can "combine state monads" in {
    assert(both.run(20).value == ((42, ("Result of step1: 21", "Result of step2: 42"))))
  }

  "get" can "extract the state as a result" in {
    val getDemo = State.get[Int]

    assert(getDemo.run(5).value == ((5, 5)))
  }

  "set" can "update the state and return unit as the result" in {
    val setDemo = State.set[Int](30)

    assert(setDemo.run(15).value == ((30, ())))
  }

  "pure" can "ignore state and return the supplied result" in {
    val pureDemo = State.pure[Int, String]("Result")

    assert(pureDemo.run(12).value == ((12, "Result")))
  }

  "inspect" can "extract the state via a transformation function" in {
    val inspectDemo = State.inspect[Int, String](x => s"$x!")

    assert(inspectDemo.run(10).value == ((10, "10!")))
  }

  "modify" can "update the state via a function" in {
    val modifyDemo = State.modify[Int](_ + 1)

    assert(modifyDemo.run(12).value == ((13, ())))
  }

  "program" can "manipulate state" in {
    assert(program.run(1).value == ((3, (1, 2, 3000))))
  }
}