package scala_cats.chapter07

import cats.Applicative
import cats.implicits.catsSyntaxTuple2Semigroupal

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object TraverseWorkout {
  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(host: String): Future[Int] = Future(host.length * 60)

  // Motivating example
  val allUptimes: Future[List[Int]] = hostnames.foldLeft(Future(List.empty[Int])) {
    (accum, host) =>
      val uptime = getUptime(host)
      for {
        accum <- accum
        uptime <- uptime
      } yield accum :+ uptime
  }

  // Extract collection and getUptime function as input parameters, and generalise types
  // This leads to implementation of Future.traverse, which is similar to;
  def traverse[A, B](values: List[A])
                    (func: A => Future[B]): Future[List[B]] =
    values.foldLeft(Future(List.empty[B])) { (accum, host) =>
      val item = func(host)
      for {
        accum <- accum
        item <- item
      } yield accum :+ item
    }

  // Now look at extracting the combine logic
  // Non-generalised types first
  def combine(accum: Future[List[Int]],
              host: String
  ): Future[List[Int]] = {
    val uptime = getUptime(host)
    for {
      accum <- accum
      uptime <- uptime
    } yield accum :+ uptime
  }

  val allUptimes2: Future[List[Int]] = hostnames.foldLeft(Future(List.empty[Int]))(combine)

  // Now generalise the types
  def combine2[A](accum: Future[List[A]],
                  value: Future[A]
  ): Future[List[A]] = {
    for {
      accum <- accum
      value <- value
    } yield accum :+ value
  }

  def traverse2[A, B](values: List[A])
                     (func: A => Future[B]): Future[List[B]] =
    values.foldLeft(Future(List.empty[B])) { (accum, host) =>
      combine2(accum, func(host))
    }

  // Now generalise the Future type
  def combine3[F[_]: Applicative, A](accum: F[List[A]],
                                     value: F[A]
                                    ): F[List[A]] = {
    (accum, value).mapN(_ :+ _)
  }

  import cats.syntax.applicative._ // for pure

  def traverse3[F[_]: Applicative, A, B](values: List[A])
                                        (func: A => F[B]): F[List[B]] =
    values.foldLeft(List.empty[B].pure[F]) { (accum, host) =>
      combine3(accum, func(host))
    }

  // And finally, with substitution
  def listTraverse[F[_]: Applicative, A, B](values: List[A])
                                           (func: A => F[B]): F[List[B]] =
    values.foldLeft(List.empty[B].pure[F]) { (accum, host) =>
      (accum, func(host)).mapN(_ :+ _)
    }

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] = listTraverse(list)(identity)

  // And abstracting over collection
  trait MyTraverse[F[_]] {
    def traverse[G[_] : Applicative, A, B](values: F[A])(func: A => G[B]): G[F[B]]

    def sequence[G[_] : Applicative, B](values: F[G[B]]): G[F[B]] = traverse(values)(identity)
  }

  // Cats traverse
  import cats.Traverse
  import cats.instances.future._ // for Applicative
  import cats.instances.list._   // for Traverse

  val allUptimes3: Future[List[Int]] = Traverse[List].traverse(hostnames)(getUptime)

  val numbers = List(Future(1), Future(2), Future(3))

  val sequencedNumbers: Future[List[Int]] = Traverse[List].sequence(numbers)
}