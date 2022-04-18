package scala_cats.casestudies.cdrt.take3

import cats.kernel.{CommutativeMonoid, Semigroup}
import cats.syntax.semigroup._ // for |+|

class MyMap[K, V](val list: List[(K, V)]) {
  def put(k: K, v: V): MyMap[K, V] =
    putWithCombine(k, v)((_, vNew) => vNew)

  def putWithCombine(k: K, v: V)(f: (V, V) => V): MyMap[K, V] = {
    getWithIndex(k) match {
      case None =>
        MyMap(list :+ (k -> v))
      case Some((vOld, idx)) =>
        MyMap(list.updated(idx, (k, f(vOld, v))))
    }
  }

  def get(k: K): Option[V] = list.iterator.find(_._1 == k).map(_._2)

  // Only public so it can be tested
  def getWithIndex(k: K): Option[(V, Int)] = list.indexWhere(_._1 == k) match {
    case -1 => Option.empty[(V, Int)]
    case i => Option((list(i)._2, i))
  }

  def getOrElse(k: K, default: => V): V = list.iterator.find(_._1 == k).map(_._2).getOrElse(default)

  def values: List[V] = list.map(_._2)

  // not bothering with hashCode!
  def canEqual(a: Any): Boolean = a.isInstanceOf[MyMap[K, V]]

  override def equals(that: Any): Boolean =
    that match {
      case that: MyMap[K, V] => {
        that.canEqual(this) &&
          this.list == that.list
      }
      case _ => false
    }

  override def toString: String = list.toString()
}

object MyMap {
  def apply[K, V](l: List[(K, V)] = List.empty) = new MyMap[K, V](l)

  // Required for take3
  implicit def myMapSemigroup[K, V](implicit sgv: Semigroup[V]): Semigroup[MyMap[K, V]] =
    (x: MyMap[K, V], y: MyMap[K, V]) => y.list.foldLeft(MyMap(x.list)) {
    case (m, kv) =>
      val (k, v) = kv
      m.putWithCombine(k, v)(_ |+| _)
  }

  // Required for take4, GCounter.gCounterInstance
  implicit def commutativeMonoid[K, V](implicit sgv: Semigroup[V]): CommutativeMonoid[MyMap[K, V]] = new CommutativeMonoid[MyMap[K, V]] {
    override def empty: MyMap[K, V] = MyMap()

    override def combine(x: MyMap[K, V], y: MyMap[K, V]): MyMap[K, V] = {
      // Note that following doesn't work, as CommutativeMonoid[MyMap[K, V]] is itself a Semigroup[MyMap[K, V]]
      // Semigroup[MyMap[K, V]].combine(x, y)

      val sg: Semigroup[MyMap[K, V]] = myMapSemigroup
      sg.combine(x, y)
    }
  }
}