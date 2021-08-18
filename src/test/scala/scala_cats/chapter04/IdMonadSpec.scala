package scala_cats.chapter04

import cats.Monad
import scala_cats.UnitSpec

class IdMonadSpec extends UnitSpec {
  "Id" can "support monadic and functor ops" in {
    import cats.Id

    val a = Monad[Id].pure(3)
    assert(a === (3: Id[Int]))

    val b = Monad[Id].flatMap(a)(_ + 1)
    assert(b === (4: Id[Int]))

    import cats.syntax.flatMap._
    import cats.syntax.functor._ // for flatMap

    val c = for {
      x <- a
      y <- b
    } yield x + y
    assert(c === (7: Id[Int]))
  }
}
