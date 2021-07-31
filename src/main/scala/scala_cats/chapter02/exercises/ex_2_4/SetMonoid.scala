package scala_cats.chapter02.exercises.ex_2_4

import scala_cats.chapter02.Monoid

//noinspection MutatorLikeMethodIsParameterless
object SetMonoid {
  // NOTE: Adding empty brackets for these methods fixes code inspection warnings BUT
  //       prevents automatic implicit derivation
  implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty

    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  implicit def setAddMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty

    override def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
  }

  // intersect - not a monoid, no identity element

  implicit def setSymmetricDifferenceMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty

    override def combine(x: Set[A], y: Set[A]): Set[A] = (x diff y) union (y diff x)
  }
}