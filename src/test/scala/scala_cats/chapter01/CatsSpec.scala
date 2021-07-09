package scala_cats.chapter01

import cats.Show
import scala_cats.UnitSpec
import cats.instances.int._
import cats.instances.string._
import scala_cats.chapter01.exercises.ex_1_3.{Cat, Printable}
import scala_cats.chapter01.exercises.ex_1_4_6.ShowCat

class CatsSpec extends UnitSpec {
  val cat = Cat("Tigger", 6, "tabby")

  "printable" can "format a cat" in {
    assert(Printable.format(cat) == "Tigger is a 6 year-old tabby cat")

    Printable.print(cat)
  }

  "printableOps" can "format a cat more easily" in {
    import scala_cats.chapter01.exercises.ex_1_3.PrintableSyntax.PrintableOps

    assert(cat.format == "Tigger is a 6 year-old tabby cat")

    cat.print
  }

  "show" can "show an int" in {
    val showInt: Show[Int] = Show.apply[Int]
    assert(showInt.show(123) == "123")
  }

  it can "show a string" in {
    val showString: Show[String] = Show.apply[String]
    assert(showString.show("abc") == "abc")
  }

  it can "show a cat even more easily" in {
    import cats.syntax.show._

    val cat = ShowCat("Ernie", 3, "black")
    import ShowCat.showCat
    assert(cat.show == "Ernie is a 3 year-old black cat")
  }
}