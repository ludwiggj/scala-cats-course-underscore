package scala_cats.chapter02.exercises.ex_2_4

import scala_cats.chapter02.Semigroup

object SetSemigroup {
  implicit def setUnionSemigroup[A](): Semigroup[Set[A]] = (x: Set[A], y: Set[A]) => x union y

  implicit def setAddSemigroup[A](): Semigroup[Set[A]] = (x: Set[A], y: Set[A]) => x ++ y

  implicit def setIntersectSemigroup[A](): Semigroup[Set[A]] = (x: Set[A], y: Set[A]) => x intersect y

  implicit def setDifferenceNotASemigroup[A](): Semigroup[Set[A]] = (x: Set[A], y: Set[A]) => x diff y

  implicit def setSymmetricDifferenceSemigroup[A](): Semigroup[Set[A]] =
    (x: Set[A], y: Set[A]) => (x diff y) union (y diff x)
}