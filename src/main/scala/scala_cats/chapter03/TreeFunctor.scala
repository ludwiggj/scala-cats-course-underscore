package scala_cats.chapter03

import cats.Functor

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def leaf[A](value: A): Tree[A] = Leaf(value)
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
}

object TreeFunctor {
  implicit val treeFunctor = new Functor[Tree] {
    override def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(lta, rta) => Branch(map(lta)(f), map(rta)(f))
    }
  }
}
