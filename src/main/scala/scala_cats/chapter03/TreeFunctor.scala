package scala_cats.chapter03

import cats.Functor

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def leaf[A](value: A): Tree[A] = Leaf(value)

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  import cats.Eq

  implicit def eqTree[T: Eq]: Eq[Tree[T]] = Eq.instance[Tree[T]] { (t1, t2) =>
    (t1, t2) match {
      case (Leaf(v1), Leaf(v2)) =>
        Eq[T].eqv(v1, v2)
      case (Branch(t1Left, t1Right), Branch(t2Left, t2Right)) =>
        Eq[Tree[T]].eqv(t1Left, t2Left) && Eq[Tree[T]].eqv(t1Right, t2Right)
      case _ => false
    }
  }
}

object TreeFunctor {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(lta, rta) => Branch(map(lta)(f), map(rta)(f))
    }
  }
}
