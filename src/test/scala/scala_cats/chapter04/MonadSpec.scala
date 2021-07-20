package scala_cats.chapter04

import scala_cats.UnitSpec
import cats.Monad
import scala.concurrent.{Await, Future}

//noinspection OptionEqualsSome
class MonadSpec extends UnitSpec {

  "cats monad" can "supports option" in {
    val opt1 = Monad[Option].pure(3)
    assert(opt1 === Some(3))

    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
    assert(opt2 === Some(5))

    val opt3 = Monad[Option].map(opt2)(a => 100 * a)
    assert(opt3 === Some(500))

    assert(Monad[Option].flatMap(Option(1))(a => Option(a * 2)) === Some(2))
  }

  it can "support list" in {
    val list1 = Monad[List].pure(3)
    assert(list1 === List(3))

    val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
    assert(list2 === List(1, 10, 2, 20, 3, 30))

    val list3 = Monad[List].map(list2)(a => a + 123)
    assert(list3 === List(124, 133, 125, 143, 126, 153))
  }

  it can "support vector" in {
    assert(Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10)) === Vector(1, 10, 2, 20, 3, 30))
  }

  it can "support future" in {
    import scala.concurrent.duration.DurationInt
    import scala.concurrent.ExecutionContext.Implicits.global
    val fm = Monad[Future]
    val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))

    assert(Await.result(future, 1.second) === 3)
  }

  "pure" can "create appropriate types" in {
    import cats.syntax.applicative._

    assert(1.pure[Option] === Some(1))

    assert(1.pure[List] === List(1))
  }

  object MonadOps {
    import cats.syntax.functor._ // for map
    import cats.syntax.flatMap._ // for flatMap

    def sumSquareDesugared[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
      a.flatMap(x => b.map(y => x*x + y*y))
    }

    def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
      for {
        x <- a
        y <- b
      } yield x*x + y*y
    }
  }

  "sumSquare" can "add up sum of squares" in {
    import MonadOps.sumSquareDesugared
    assert(sumSquareDesugared(Option(3), Option(4)) === Some(25))
    assert(sumSquareDesugared(List(1, 2, 3), List(4, 5)) === List(17, 26, 20, 29, 25, 34))
  }

  it can "be expressed by a for comprehension" in {
    import MonadOps.sumSquare
    assert(sumSquare(Option(3), Option(4)) === Some(25))
    assert(sumSquare(List(1, 2, 3), List(4, 5)) === List(17, 26, 20, 29, 25, 34))
  }

  "Id" can "support monadic and functor ops" in {
    import cats.Id

    val a = Monad[Id].pure(3)
    assert(a === (3: Id[Int]))

    val b = Monad[Id].flatMap(a)(_ + 1)
    assert(b === (4: Id[Int]))

    import cats.syntax.functor._ // for map
    import cats.syntax.flatMap._ // for flatMap

    val c = for {
      x <- a
      y <- b
    } yield x + y
    assert(c === (7: Id[Int]))
  }

  "IdMonadOps" can "implement pure" in {
    import scala_cats.chapter04.ex_4_3_1.IdMonadOps.pure
    assert(pure(123) === 123)
  }

  it can "implement map" in {
    import scala_cats.chapter04.ex_4_3_1.IdMonadOps.{pure, map}
    assert(map(pure(123))(_ * 2) === 246)
  }

  it can "implement flatmap" in {
    import scala_cats.chapter04.ex_4_3_1.IdMonadOps.{pure, flatMap}
    assert(flatMap(pure(123))(_ * 2) === 246)
  }

  "either monad" can "make either nicer" in {
    import cats.syntax.either._

    val a = 3.asRight[String]
    val b = 4.asRight[String]

    val res = for {
      x <- a
      y <- b
    } yield x*x + y*y

    assert(res === Right(25))
  }
}