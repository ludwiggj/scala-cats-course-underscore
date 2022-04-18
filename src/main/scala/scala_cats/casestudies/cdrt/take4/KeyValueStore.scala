package scala_cats.casestudies.cdrt.take4

import scala_cats.casestudies.cdrt.take3.MyMap

trait KeyValueStore[F[_, _]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)

  def values[K, V](f: F[K, V]): List[V]
}

object KeyValueStore {
  implicit val mapInstance: KeyValueStore[Map] = new KeyValueStore[Map] {
    override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

    override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

    override def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  }

  // Now all we have to do is implement these operations: compare to take3.GCounter and take3.GCounterLessRestrictive
  implicit val myMapInstance: KeyValueStore[MyMap] = new KeyValueStore[MyMap] {
    override def put[K, V](f: MyMap[K, V])(k: K, v: V): MyMap[K, V] = f.put(k, v)

    override def get[K, V](f: MyMap[K, V])(k: K): Option[V] = f.get(k)

    override def values[K, V](f: MyMap[K, V]): List[V] = f.values
  }

  def apply[F[_, _]](implicit kvs: KeyValueStore[F]): KeyValueStore[F] = kvs
}