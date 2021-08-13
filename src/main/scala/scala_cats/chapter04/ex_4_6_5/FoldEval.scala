package scala_cats.chapter04.ex_4_6_5

import cats.Eval

object FoldEval {
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

  def foldRightSafe[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(foldRightSafe(tail, acc)(fn).map(fn(head, _)))
      case Nil =>
        Eval.now(acc)
    }

  def foldRightTextbookEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightTextbookEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  def foldRightTextbook[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    foldRightTextbookEval(as, Eval.now(acc)) {
      (a, evalB) => evalB.map(fn(a, _))
    }
}
