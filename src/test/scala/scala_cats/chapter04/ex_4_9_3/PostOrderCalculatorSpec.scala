package scala_cats.chapter04.ex_4_9_3

import scala_cats.UnitSpec
import scala_cats.chapter04.ex_4_9_3.PostOrderCalculator._

class PostOrderCalculatorSpec extends UnitSpec {

  "PostOrderCalculator" can "evaluate a single symbol" in {
    assert(evalOne("42").runA(Nil).value == 42)
    assert(evalOneTextbook("42").runA(Nil).value == 42)
  }

  it can "returns result of latest symbol" in {
    assert(evalOne("42").flatMap(_ => evalOne("5")).runA(Nil).value == 5)
    assert(evalOneTextbook("42").flatMap(_ => evalOne("5")).runA(Nil).value == 5)
  }

  it can "add two numbers together" in {
    val result = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    } yield ans

    assert(result.runA(Nil).value == 3)

    val resultTextbook = for {
      _ <- evalOneTextbook("1")
      _ <- evalOneTextbook("2")
      ans <- evalOneTextbook("+")
    } yield ans

    assert(resultTextbook.runA(Nil).value == 3)
  }

  "evalAll" can "evaluate a larger sum" in {
    assert(evalAll(List("1", "2", "+", "3", "*")).runA(Nil).value == 9)
    assert(evalAll2(List("1", "2", "+", "3", "*")).runA(Nil).value == 9)
    assert(evalAllTextbook(List("1", "2", "+", "3", "*")).runA(Nil).value == 9)
  }

  it can "evaluate a single term" in {
    assert(evalAll(List("1")).runA(Nil).value == 1)
    assert(evalAll2(List("1")).runA(Nil).value == 1)
    assert(evalAllTextbook(List("1")).runA(Nil).value == 1)
  }

  it can "evaluate an empty list" in {
    assert(evalAllTextbook(List()).runA(Nil).value == 0)
  }

  it can "be mixed with evalOne" in {
    val result = for {
      _ <- evalAll(List("1", "2", "+"))
      _ <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans

    assert(result.runA(Nil).value == 21)
  }

  "evalInput" can "return single digit" in {
    assert(evalInput("31") == 31)
  }

  it can "calculate a sum" in {
    assert(evalInput("1 2 + 3 4 + *") == 21)
  }
}