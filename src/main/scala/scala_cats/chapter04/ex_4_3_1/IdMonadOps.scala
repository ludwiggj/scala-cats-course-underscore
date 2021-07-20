package scala_cats.chapter04.ex_4_3_1

object IdMonadOps {
  import cats.Id

  def pure[A](value: A): Id[A] = value

  def map[A, B](initial: Id[A])(func: A => B): Id[B] = func(initial)

  def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = func(initial)
}