package scala_cats.chapter07

import scala_cats.UnitSpec
import scala_cats.chapter07.TraverseWorkout._
import scala_cats.chapter07.ex_7_2_2_2.TraverseOption
import scala_cats.chapter07.ex_7_2_2_3.TraverseValidated

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

import cats.data.Validated

class TraverseSpec extends UnitSpec {
  "manual traverse" can "produce single future" in {
    assert(Await.result(allUptimes, 1.second) == List(1020, 960, 840))
    assert(Await.result(allUptimes2, 1.second) == List(1020, 960, 840))
  }

  "future traverse" can "produce single future" in {
    assert(Await.result(traverse(hostnames)(getUptime), 1.second) == List(1020, 960, 840))
    assert(Await.result(traverse2(hostnames)(getUptime), 1.second) == List(1020, 960, 840))
    assert(Await.result(traverse3(hostnames)(getUptime), 1.second) == List(1020, 960, 840))
    assert(Await.result(listTraverse(hostnames)(getUptime), 1.second) == List(1020, 960, 840))
    assert(Await.result(Future.traverse(hostnames)(getUptime), 1.second) == List(1020, 960, 840))
    assert(Await.result(allUptimes3, 1.second) == List(1020, 960, 840))

    // Traverse syntax
    import cats.syntax.traverse._ // for sequence and traverse
    assert(Await.result(hostnames.traverse(getUptime), 1.second) == List(1020, 960, 840))
  }

  "list sequence" can "solve exercise 7.2.2.1" in {
    assert(listSequence(List(Vector(1, 2), Vector(3, 4))) == Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4)))
    assert(listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) ==
      Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))
    )
  }

  "option traverse" can "return some if all elements are somes" in {
    assert(TraverseOption.process(List(2, 4, 6)) == Some(List(2, 4, 6)))
  }

  it can "return none if any element is a none" in {
    assert(TraverseOption.process(List(1, 2, 3)) == None)
  }

  "validated traverse" can "return valid if all elements are valid" in {
    assert(TraverseValidated.process(List(2, 4, 6)) == Validated.valid(List(2, 4, 6)))
  }

  it can "return all errors if any element is not valid" in {
    assert(TraverseValidated.process(List(1, 2, 3)) == Validated.invalid(List("1 is not even", "3 is not even")))
  }

  "sequence" can "sequence a list of futures" in {
    assert(Await.result(sequencedNumbers, 1.second) == List(1, 2, 3))

    // Traverse syntax
    import cats.syntax.traverse._ // for sequence and traverse
    assert(Await.result(List(Future(1), Future(2), Future(3)).sequence, 1.second) == List(1, 2, 3))
  }
}