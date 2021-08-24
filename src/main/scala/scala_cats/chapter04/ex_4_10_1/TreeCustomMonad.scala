package scala_cats.chapter04.ex_4_10_1

import cats.Monad
import Tree._

object TreeCustomMonad {

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {

    override def flatMap[A, B](tree: Tree[A])
                              (f: A => Tree[B]): Tree[B] =
      tree match {
        case Leaf(a) => f(a)
        case Branch(left, right) => Branch(
          flatMap(left)(f),
          flatMap(right)(f)
        )
      }

    override def pure[A](a: A): Tree[A] =
      Leaf(a)

    // TODO This is not @tailrec
    override def tailRecM[A, B](a: A)
                               (fn: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(fn(a)) {
        case Left(a1) => tailRecM(a1)(fn)
        case Right(b) => Leaf(b)
      }
  }
}
