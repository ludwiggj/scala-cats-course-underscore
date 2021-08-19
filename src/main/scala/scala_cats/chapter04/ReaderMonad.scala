package scala_cats.chapter04

import cats.data.Reader

object ReaderMonad {
  final case class Cat(name: String, favouriteFood: String)

  val catNameReader: Reader[Cat, String] = Reader(cat => cat.name)

  val greetKittyReader: Reader[Cat, String] = catNameReader.map(name => s"Hello $name")

  val feedKittyReader: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favouriteFood}")

  val greetAndFeedReader: Reader[Cat, String] =
    for {
      greet <- greetKittyReader
      feed <- feedKittyReader
    } yield s"$greet. $feed."
}
