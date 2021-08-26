package scala_cats.chapter05

import cats.conversions.all.autoWidenFunctor
import cats.implicits.toFlatMapOps

object MonadComposition {
  case class User(name: String)

  def lookupUser(l: Long): Either[Error, Option[User]] = ???

  def lookupUserName(id: Long): Either[Error, Option[String]] =
    for {
      optUser <- lookupUser(id)
    } yield {
      for {user <- optUser} yield user.name
    }

  import cats.syntax.applicative._ // for pure
  import cats.Monad

  // Hypothetical example. This won't actually compile:
  def compose[M1[_] : Monad, M2[_] : Monad] = {
    type Composed[A] = M1[M2[A]]
    new Monad[Composed] {
      def pure[A](a: A): Composed[A] =
        a.pure[M2].pure[M1]

      def flatMap[A, B](fa: Composed[A])
                       (f: A => Composed[B]): Composed[B] =
      // Problem! How do we write flatMap?
        ???

      override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = ???
    }
  }

   def compose2[M1[_] : Monad] = {
    type Composed[A] = M1[Option[A]]
    new Monad[Composed] {
      def pure[A](a: A): Composed[A] =
        a.pure[Option].pure[M1]

      def flatMap[A, B](fa: M1[Option[A]])
                       (f: A => Composed[B]): Composed[B] =
        fa.flatMap(_.fold[M1[Option[B]]](None.pure[M1])(f))

      override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = ???
    }
  }

}