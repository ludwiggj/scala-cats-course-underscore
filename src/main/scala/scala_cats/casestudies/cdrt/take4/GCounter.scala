package scala_cats.casestudies.cdrt.take4

import cats.kernel.CommutativeMonoid
import Helpers.KvsOps
import cats.syntax.semigroup._
import cats.syntax.foldable._
import scala_cats.casestudies.cdrt.take2.BoundedSemiLattice

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]

  def merge(f: F[K, V])(g: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GCounter {
  implicit def gCounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F], km: CommutativeMonoid[F[K, V]]): GCounter[F, K, V] =
    new GCounter[F, K, V] {
      override def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V] = {
        val newAmount = f.getOrElse(k, m.empty) |+| v
        f.put(k, newAmount)
      }

      // implicit CommutativeMonoid[F[K, V]] needed for f |+| g i.e. implicit hint is catsSyntaxSemigroup(f)(km)
      override def merge(f: F[K, V])(g: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] = f |+| g

      override def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V = f.values.combineAll
    }

  def apply[F[_, _], K, V](implicit g: GCounter[F, K, V]): GCounter[F, K, V] = g
}