package scala_cats.chapter07

import FoldingWorkout._
import scala_cats.UnitSpec
import scala_cats.chapter07.ex_7_1_2.Folder

class FoldingSpec extends UnitSpec {
  "show" can "display empty list" in {
    assert(show(Nil) == "nil")
  }

  it can "display non-empty list" in {
    assert(show(List(1, 2, 3)) == "3 then 2 then 1 then nil")
  }

  "list folder" can "reverse a list with foldLeft" in {
    assert(Folder.foldLeft(List(1, 2, 3, 4)) == List(4, 3, 2, 1))
  }

  it can "maintain list order with foldRight" in {
    assert(Folder.foldRight(List(1, 2, 3, 4)) == List(1, 2, 3, 4))
  }

  "fold right" can "implement map" in {
    assert(Folder.map(List(1, 2, 3, 4))(_ + 1) == List(2, 3, 4, 5))
  }

  it can "implement flatMap" in {
    assert(Folder.flatMap(List(1, 2, 3, 4))(i => List(i * 4, i * 4 + 1)) == List(4, 5, 8, 9, 12, 13, 16, 17))
  }

  it can "implement filter" in {
    assert(Folder.filter(List(1, 2, 3, 4))(_ % 2 == 0) == List(2, 4))
  }

  it can "implement sum" in {
    assert(Folder.sum(List(1, 2, 3, 4)) == 10)

    import cats.instances.int._
    assert(Folder.sum2(List(1, 2, 3, 4)) == 10)

    assert(Folder.sum3(List(1, 2, 3, 4)) == 10)
  }

  import cats.Foldable

  "foldable" can "fold a list of ints" in {
    import cats.instances.list._
    val ints = List(1, 2, 3)
    assert(Foldable[List].foldLeft(ints, 0)(_ + _) == 6)
  }

  it can "fold an option of int" in {
    import cats.instances.option._

    val maybeInt = Option(2)

    assert(Foldable[Option].foldLeft(maybeInt, 10)(_ * _) == 20)

    // Surprising!
    assert(Foldable[Option].foldLeft(Option.empty[Int], 10)(_ + _) == 10)
  }

  it can "implement nonEmpty" in {
    assert(Foldable[Option].nonEmpty(Option(42)))
  }

  it can "implement find" in {
    assert(Foldable[List].find(List(1, 2, 3))(_ % 2 == 0).contains(2))
  }

  "unsafesum" can "blow up" in {
    assert(FoldableWorkout.unsafeSum == 5000050000L)
  }

  "safesum" can "add up" in {
    assert(FoldableWorkout.safeSum.value == 5000050000L)
  }

  "foldable with monoid" can "combine a list" in {
    assert(Foldable[List].combineAll(List(1, 2, 3)) == 6)
  }

  it can "combine a list of vectors" in {
    import cats.instances.vector._ // for Monoid[Vector]

    val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
    assert((Foldable[List] compose Foldable[Vector]).combineAll(ints) == 21)
  }

  it can "foldMap a list" in {
    import cats.instances.string._

    assert(Foldable[List].foldMap(List(1, 2, 3))(_.toString) == "123")
  }

  "foldable syntax" can "do things more succinctly" in {
    import cats.syntax.foldable._

    assert(List(1, 2, 3).combineAll == 6)
    assert(List(1, 2, 3).foldMap(_.toString) == "123")
  }
}