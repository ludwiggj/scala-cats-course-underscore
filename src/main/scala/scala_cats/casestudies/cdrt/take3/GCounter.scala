package scala_cats.casestudies.cdrt.take3

import cats.kernel.CommutativeMonoid
import cats.syntax.semigroup._ // for |+|
import cats.syntax.foldable._  // for combineAll
import scala_cats.casestudies.cdrt.take2.BoundedSemiLattice

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]

  def merge(f: F[K, V])(g: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GCounter {
  def apply[F[_, _], K, V](implicit g: GCounter[F, K, V]): GCounter[F, K, V] = g

  implicit def mapInstance[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {
    override def increment(f: Map[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
      val newAmount = f.getOrElse(k, m.empty) |+| v
      f + (k -> newAmount)
    }

    override def merge(f: Map[K, V])(g: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
      f |+| g

    override def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V =
      f.values.toList.combineAll
  }

  // The implementation strategy for the type class instance is a bit unsatisfying.
  // Although the structure of the implementation will be the same for most
  // instances we define, we won’t get any code reuse.
  implicit def myMapInstance[K, V]: GCounter[MyMap, K, V] = new GCounter[MyMap, K, V] {
    override def increment(f: MyMap[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): MyMap[K, V] = {
      val newAmount = f.getOrElse(k, m.empty) |+| v
      f.put(k, newAmount)
    }

    // Requires Semigroup[MyMap[K, V]]
    override def merge(f: MyMap[K, V])(g: MyMap[K, V])(implicit b: BoundedSemiLattice[V]): MyMap[K, V] =
      f |+| g

    override def total(f: MyMap[K, V])(implicit m: CommutativeMonoid[V]): V =
      f.values.combineAll
  }
}