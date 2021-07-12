package scala_cats.chapter02

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

object Semigroup {
  def apply[A](implicit semigroup: Semigroup[A]): Semigroup[A] =
    semigroup

  def associativeLaw[A](x: A, y: A, z: A)
                       (implicit s: Semigroup[A]): Boolean = {
    s.combine(x, s.combine(y, z)) == s.combine(s.combine(x, y), z)
  }
}