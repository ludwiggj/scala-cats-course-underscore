package scala_cats.chapter01

import scala_cats.UnitSpec
import scala_cats.chapter01.exercises.ex_1_3.{Cat, Printable}

class ExercisesSpec extends UnitSpec {
  val cat = Cat("Tigger", 6, "Tabby")

  "cat" can "print" in {
    assert(Printable.format(cat) == "Tigger is a 6 year-old Tabby cat")

    Printable.print(cat)
  }

  it can "print more easily" in {
    import scala_cats.chapter01.exercises.ex_1_3.PrintableSyntax.PrintableOps

    assert(cat.format == "Tigger is a 6 year-old Tabby cat")

    cat.print
  }
}