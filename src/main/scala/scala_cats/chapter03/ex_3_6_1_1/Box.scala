package scala_cats.chapter03.ex_3_6_1_1

import cats.Eq
import scala_cats.chapter03.ex_3_6_1_1.ContravariantFunctor.Printable

case class Box[A](value: A)

object Box {
  object Version1 {
    // This version just depends on calling toString on the value in the Box
    def printableA[A]: Printable[A] = (value: A) => value.toString

    implicit def printableBox[A]: Printable[Box[A]] =
      printableA.contramap((b: Box[A]) => b.value)
  }

  object Version2 {
    // single abstract method version
    implicit def printableBox[A](implicit p: Printable[A]): Printable[Box[A]] =
      (b: Box[A]) => p.format(b.value)
  }

  object Version3 {
    implicit def printableBox[A](implicit p: Printable[A]): Printable[Box[A]] =
      p.contramap((b: Box[A]) => b.value)
  }

  object Textbook {
    implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
      p.contramap[Box[A]](_.value)
  }

  object OptionOfBox {
    // Can't use contramap to translate an Option[Box[A]] to an A, as cannot translate None to an A
//    implicit def printableOptionBox[A](implicit p: Printable[A]): Printable[Option[Box[A]]] =
//      p.contramap {
//        case Some(b) => b.value
//        case None => "None"     // can't handle this case!
//      }

    // i.e. cannot use contramap if source is A and target is Option[A], or Option[Something[A]]

    // But can translate an Option[Box[A]] to an Option[A]
    implicit def printableOptionBox[A](implicit p: Printable[Option[A]]): Printable[Option[Box[A]]] =
      p.contramap {
        case Some(b) => Some(b.value)
        case None => None
      }
  }

  implicit def eqBox[A: Eq]: Eq[Box[A]] = Eq.instance[Box[A]] { (b1, b2) =>
    Eq[A].eqv(b1.value, b2.value)
  }
}