package scala_cats.chapter02.exercises.ex_2_5_4

import cats.Monoid
import cats.syntax.semigroup._ // for |+|

case class Order(totalCost: Double, quantity: Double)

object Order {
  import cats.Eq
  import cats.syntax.eq._

  implicit val eqOrder: Eq[Order] = Eq.instance[Order] { (o1, o2) =>
    o1.totalCost === o2.totalCost &&
      o1.quantity === o2.quantity
  }
}

object SuperAdder {
  private def addList[T](items: List[T])(implicit m: Monoid[T]): T =
    items.foldLeft(m.empty)(m.combine)

  private def addListTextbook[T: Monoid](items: List[T]): T =
    items.foldLeft(Monoid[T].empty)(_ |+| _)

  def add(items: List[Int]): Int = {
    import cats.instances.int._
    addList(items)
  }

  def add(items: List[Option[Int]]): Option[Int] = {
    import cats.instances.int._
    import cats.instances.option._
    addListTextbook(items)
  }

  def add(items: List[Order]): Double = {
    import cats.instances.double._
    addList(items.map(_.totalCost))
  }

  def addOrdersTextbook(items: List[Order]): Order = {
    implicit val m: Monoid[Order] = new Monoid[Order] {
      override def empty: Order = Order(0, 0)

      override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    }

    addList(items)
  }
}
