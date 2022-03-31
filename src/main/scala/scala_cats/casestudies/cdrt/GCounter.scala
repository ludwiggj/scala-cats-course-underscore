package scala_cats.casestudies.cdrt

import cats.Monoid
import cats.kernel.Semigroup
import cats.syntax.semigroup._
import cats.instances.int._

final case class GCounter(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int): GCounter = {
    val newAmount = counters.getOrElse(machine, 0) + amount
    GCounter(counters + (machine -> newAmount))
  }

  def merge(that: GCounter): GCounter = {
    implicit val sg = new Semigroup[Map[String, Int]] {
      override def combine(x: Map[String, Int], y: Map[String, Int]): Map[String, Int] =
        x ++ y.map {
          case (yk, yv) =>
            yk -> Math.max(x.getOrElse(yk, 0), yv) // assumes all counts are positive
        }
    }

    this.copy(counters = counters |+| that.counters)
  }

  def total: Int = counters.values.foldLeft(Monoid[Int].empty)(_ |+| _)

  def mergeTextbook(that: GCounter): GCounter =
    GCounter(that.counters ++ this.counters.map {
      case (k, v) =>
        k -> (v max that.counters.getOrElse(k, 0))
    })

  def totalTextbook: Int =
    counters.values.sum
}
