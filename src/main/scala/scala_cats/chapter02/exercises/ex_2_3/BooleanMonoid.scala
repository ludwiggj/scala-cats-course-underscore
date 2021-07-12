package scala_cats.chapter02.exercises.ex_2_3

import scala_cats.chapter02.Monoid

object BooleanMonoid {
  object BooleanOps {
    implicit val booleanAndMonoid = new Monoid[Boolean] {
      override def empty: Boolean = true

      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

    implicit val booleanOrMonoid = new Monoid[Boolean] {
      override def empty: Boolean = false

      override def combine(x: Boolean, y: Boolean): Boolean = x || y
    }

    implicit val booleanXorMonoid = new Monoid[Boolean] {
      override def empty: Boolean = false

      override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
    }

    implicit val booleanXnorMonoid = new Monoid[Boolean] {
      override def empty: Boolean = true

      override def combine(x: Boolean, y: Boolean): Boolean = (x || !y) && (!x || y)
    }
  }
}