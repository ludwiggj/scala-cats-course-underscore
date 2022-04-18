package scala_cats.casestudies.cdrt.take2

import cats.kernel.CommutativeMonoid
import cats.syntax.semigroup._ // for |+|

final case class GenericGCounter[A](counters: Map[String, A]) {
  def increment(machine: String, value: A)(implicit cm: CommutativeMonoid[A]): GenericGCounter[A] = {
    val newValue = counters.getOrElse(machine, cm.empty) |+| value
    GenericGCounter(counters + (machine -> newValue))
  }

  def total(implicit cm: CommutativeMonoid[A]): A = counters.values.foldLeft(cm.empty)(_ |+| _)

  def merge(that: GenericGCounter[A])(implicit bsl: BoundedSemiLattice[A]): GenericGCounter[A] =
    GenericGCounter(that.counters ++ this.counters.map {
      case (k, v) =>
        k -> (v |+| that.counters.getOrElse(k, bsl.empty))
    })
}