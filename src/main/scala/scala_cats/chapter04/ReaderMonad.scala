package scala_cats.chapter04

import cats.data.Reader

object ReaderMonad {
final case class Cat(name: String, favouriteFood: String)

  val catName: Reader[Cat, String] = Reader(cat => cat.name)
}
