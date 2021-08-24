package scala_cats.chapter04.ex_4_10_1

import cats.Monad
import scala.annotation.tailrec
import Tree._

object TreeCustomMonadStackSafe {
  implicit val treeMonad = new Monad[Tree] {
    def pure[A](value: A): Tree[A] =
      Leaf(value)

    def flatMap[A, B](tree: Tree[A])(func: A => Tree[B]): Tree[B] =
      tree match {
        case Branch(l, r) =>
          Branch(flatMap(l)(func), flatMap(r)(func))
        case Leaf(value) =>
          func(value)
      }

    // It involves an explicit depth first traversal of the tree,
    // maintaining an open list of nodes to visit and a closed
    // list of nodes to use to reconstruct the tree
    def tailRecM[A, B](arg: A)
                      (func: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(
                open: List[Tree[Either[A, B]]],
                closed: List[Option[Tree[B]]]): List[Tree[B]] =
        open match {
          case Branch(l, r) :: next =>
            loop(l :: r :: next, None :: closed)
          case Leaf(Left(value)) :: next =>
            loop(func(value) :: next, closed)
          case Leaf(Right(value)) :: next =>
            loop(next, Some(pure(value)) :: closed)
          case Nil =>
            closed.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
              maybeTree.map(_ :: acc).getOrElse {
                val left :: right :: tail = acc
                branch(left, right) :: tail
              }
            }
        }

      loop(List(func(arg)), Nil).head
    }
  }
}