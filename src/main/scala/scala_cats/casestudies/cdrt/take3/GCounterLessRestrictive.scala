package scala_cats.casestudies.cdrt.take3

import cats.kernel.CommutativeMonoid
import cats.syntax.semigroup._ // for |+|
import cats.syntax.foldable._  // for combineAll
import scala_cats.casestudies.cdrt.take2.BoundedSemiLattice

trait GCounterLessRestrictive[F[_, _]] {
  def increment[K, V](f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]

  def merge[K, V](f: F[K, V])(g: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

  def total[K, V](f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GCounterLessRestrictive {
  def apply[F[_, _]](implicit g: GCounterLessRestrictive[F]): GCounterLessRestrictive[F] = g

  implicit def mapInstance: GCounterLessRestrictive[Map] = new GCounterLessRestrictive[Map] {
    override def increment[K, V](f: Map[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
      val newAmount = f.getOrElse(k, m.empty) |+| v
      f + (k -> newAmount)
    }

    override def merge[K, V](f: Map[K, V])(g: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
      f |+| g

    override def total[K, V](f: Map[K, V])(implicit m: CommutativeMonoid[V]): V =
      f.values.toList.combineAll
  }

  // The implementation strategy for the type class instance is a bit unsatisfying.
  // Although the structure of the implementation will be the same for most
  // instances we define, we won’t get any code reuse.
  implicit def myMapInstance: GCounterLessRestrictive[MyMap] = new GCounterLessRestrictive[MyMap] {
    override def increment[K, V](f: MyMap[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): MyMap[K, V] = {
      val newAmount = f.getOrElse(k, m.empty) |+| v
      f.put(k, newAmount)
    }

    // Requires Semigroup[MyMap[K, V]]
    override def merge[K, V](f: MyMap[K, V])(g: MyMap[K, V])(implicit b: BoundedSemiLattice[V]): MyMap[K, V] =
      f |+| g

    override def total[K, V](f: MyMap[K, V])(implicit m: CommutativeMonoid[V]): V =
      f.values.combineAll
  }
}