package scala_cats.chapter03.ex_3_6_1_1

import scala_cats.chapter03.ex_3_6_1_1.ContravariantFunctor.Printable

case class Box[A](value: A)

object Box {
  object Version1 {
    def printableA[A]: Printable[A] = (value: A) => value.toString

    implicit def printableBox[A]: Printable[Box[A]] =
      printableA.contramap((b: Box[A]) => b.value)
  }

  object Version2 {
    implicit def printableBox[A](implicit p: Printable[A]): Printable[Box[A]] =
      (b: Box[A]) => p.format(b.value)
  }

  object Version3 {
    implicit def printableBox[A](implicit p: Printable[A]): Printable[Box[A]] =
      p.contramap((b: Box[A]) => b.value)
  }
}