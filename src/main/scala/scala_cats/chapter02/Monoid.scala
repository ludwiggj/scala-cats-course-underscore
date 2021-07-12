package scala_cats.chapter02

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]): Monoid[A] =
    monoid

  def identityLaw[A](x: A)
                    (implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }
}