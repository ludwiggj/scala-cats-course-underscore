package scala_cats.casestudies.cdrt.take2

import cats.kernel.CommutativeMonoid
import cats.syntax.semigroup._ // for |+|
import cats.syntax.foldable._ // for combineAll

final case class GenericGCounterTextbook[A](counters: Map[String, A]) {
  def increment(machine: String, value: A)(implicit cm: CommutativeMonoid[A]): GenericGCounterTextbook[A] = {
    val newValue = counters.getOrElse(machine, cm.empty) |+| value
    GenericGCounterTextbook(counters + (machine -> newValue))
  }

  def total(implicit cm: CommutativeMonoid[A]): A = counters.values.toList.combineAll

  def merge(that: GenericGCounterTextbook[A])(implicit bsl: BoundedSemiLattice[A]): GenericGCounterTextbook[A] = {
    // import SemigroupSyntax.catsSyntaxSemigroup // This lights up, but implicit view doesn't show a difference
    GenericGCounterTextbook(that.counters |+| this.counters)
  }
}
