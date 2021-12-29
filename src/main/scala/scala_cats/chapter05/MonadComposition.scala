package scala_cats.chapter05

import cats.data.OptionT
import cats.implicits.toFlatMapOps

object MonadComposition {
  object UserExample {
    case class User(name: String)

    def lookupUser(l: Long): Either[Error, Option[User]] = {
      if (l > 0)
        Right(Some(User("Bob")))
      else
        Left(new Error(s"Cannot find user with id $l"))
    }

    def lookupUserName(id: Long): Either[Error, Option[String]] =
      for { // Either
        optUser <- lookupUser(id)
      } yield {
        for { // Option
          user <- optUser
        } yield user.name
      }

    // Alternative using OptionT - Either functionality is lost due to reducing number of type holes from 2 to 1
    type ErrorOr[A] = Either[Error, A]
    type ErrorOrOption[A] = OptionT[ErrorOr, A]

    def lookupUser2(l: Long): ErrorOrOption[User] = {
      //    ErrorOrOption[User]
      // => OptionT[ErrorOr, User]
      // => ErrorOr[Option[User]]
      // => Either[Error, Option[User]]
      if (l > 0) {
        OptionT.some(User("Bob"))
      } else {
        OptionT.none
      }
    }

    def lookupUserName2(id: Long): OptionT[ErrorOr, String] =
      lookupUser2(id).transform {
        case Some(u) => Some(u.name)
        case _ => None
      }

    def lookupUser3(l: Long): ErrorOrOption[User] = {
      //    ErrorOrOption[User]
      // => OptionT[ErrorOr, User]
      // => ErrorOr[Option[User]]
      // => Either[Error, Option[User]]

      import cats.syntax.applicative._ // for pure method
      import cats.syntax.either._      // for asLeft method

      if (l > 0) {
        User("Bob").pure[ErrorOrOption] // This works
      } else {
        // This is no good - type is  ErrorOrOption[Either[Error, Option[User]]]
        new Error(s"Cannot find user with id $l").asLeft[Option[User]].pure[ErrorOrOption]
        OptionT.none
      }
    }

    def lookupUserName3(id: Long): OptionT[ErrorOr, String] =
      lookupUser2(id).transform {
        case Some(u) => Some(u.name)
        case _ => None
      }
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

  // Fix M2 as Option, now we can do something
  def compose2[M1[_] : Monad] = {
    type Composed[A] = M1[Option[A]]
    new Monad[Composed] {
      def pure[A](a: A): Composed[A] =
        a.pure[Option].pure[M1]

      def flatMap[A, B](fa: M1[Option[A]])
                       (f: A => Composed[B]): Composed[B] = {

        val empty: M1[Option[B]] = Option.empty[B].pure[M1]

        fa.flatMap(_.fold[M1[Option[B]]](empty)(f))
      }

      override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = ???
    }
  }
}