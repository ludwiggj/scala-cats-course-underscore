package scala_cats.chapter03

import scala_cats.UnitSpec
import scala_cats.chapter03.ex_3_6_1_1.Box
import scala_cats.chapter03.ex_3_6_1_1.ContravariantFunctor.{Printable, booleanPrintable, format, stringPrintable}

class ContravariantFunctorSpec extends UnitSpec {

  "stringPrintable" can "format a string" in {
    implicit val impStringPrintable: Printable[String] = stringPrintable

    assert(format("hello") === "'hello'")
  }

  it can "produce an intPrintable" in {
    implicit val intPrintable: Printable[Int] = stringPrintable.contramap((i: Int) => i.toString)

    assert(format(65) === "'65'")
  }

  "booleanPrintable" can "format a boolean" in {
    implicit val impBooleanPrintable: Printable[Boolean] = booleanPrintable

    assert(format(true) === "yes")
    assert(format(false) === "no")
  }

  it can "produce an isEven int printable" in {
    implicit val isEvenIntPrintable: Printable[Int] = booleanPrintable.contramap((i: Int) => i % 2 == 0)

    assert(format(1) === "no")
    assert(format(2) === "yes")
  }

  "boxPrintable version 1" can "format a boxed string" in {
    import scala_cats.chapter03.ex_3_6_1_1.Box.Version1.printableBox

    assert(format(Box("hello world")) === "hello world")
  }

  it can "format a boxed boolean" in {
    import scala_cats.chapter03.ex_3_6_1_1.Box.Version1.printableBox

    assert(format(Box(true)) === "true")
  }

  "boxPrintable version 2" can "format a boxed string" in {
    import scala_cats.chapter03.ex_3_6_1_1.Box.Version2.printableBox
    implicit val impStringPrintable: Printable[String] = stringPrintable

    assert(format(Box("hello world")) === "'hello world'")
  }

  it can "format a boxed boolean" in {
    import scala_cats.chapter03.ex_3_6_1_1.Box.Version2.printableBox
    implicit val impBooleanPrintable: Printable[Boolean] = booleanPrintable

    assert(format(Box(true)) === "yes")
  }

  "boxPrintable version 3" can "format a boxed string" in {
    import scala_cats.chapter03.ex_3_6_1_1.Box.Version3.printableBox
    implicit val impStringPrintable: Printable[String] = stringPrintable

    assert(format(Box("hello world")) === "'hello world'")
  }

  it can "format a boxed boolean" in {
    import scala_cats.chapter03.ex_3_6_1_1.Box.Version3.printableBox
    implicit val impBooleanPrintable: Printable[Boolean] = booleanPrintable

    assert(format(Box(true)) === "yes")
  }

  "cats contravariant" can "show symbol" in {
    import cats.Contravariant
    import cats.Show

    val showString = Show[String]
    val showSymbol = Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}")

    assert(showSymbol.show(Symbol("Graeme")) === "'Graeme")
  }

  it can "show symbol more succinctly" in {
    import cats.syntax.contravariant._
    import cats.Show

    val showString = Show[String]
    val showSymbol = showString.contramap[Symbol](sym => s"'${sym.name}")

    assert(showSymbol.show(Symbol("Graeme")) === "'Graeme")
  }
}