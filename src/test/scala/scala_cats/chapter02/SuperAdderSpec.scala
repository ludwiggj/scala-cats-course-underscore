package scala_cats.chapter02

import scala_cats.UnitSpec
import scala_cats.chapter02.exercises.ex_2_5_4.{Order, SuperAdder}

class SuperAdderSpec extends UnitSpec {

  "SuperAdder add" can "add list of numbers" in {
    assert(SuperAdder.add(List(1, 4, 9, -2)) == 12)
  }

  it can "add empty list of numbers" in {
    assert(SuperAdder.add(List[Int]()) == 0)
  }

  it can "add list of option numbers" in {
    assert(SuperAdder.add(List(Some(2), Some(-9), None)) == Some(-7))
  }

  it can "add empty list of option numbers" in {
    assert(SuperAdder.add(List[Option[Int]]()) == None)
  }

  it can "add orders cost" in {
    assert(SuperAdder.add(List(Order(5.1, 3), Order(24.97, 1))) == 30.07)
  }

  it can "add orders" in {
    assert(SuperAdder.addOrdersTextbook(List(Order(5.1, 3), Order(24.97, 1))) == Order(30.07, 4))
  }
}