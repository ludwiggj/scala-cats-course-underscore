package scala_cats.chapter04

import scala_cats.UnitSpec
import scala_cats.chapter04.ReaderMonad.{Cat, catNameReader, greetAndFeedReader, greetKittyReader}

class ReaderMonadSpec extends UnitSpec {
  val catName = "Garfield"
  val catFood = "Lasagne"

  "Cat Reader" can "return cat's name" in {
    assert(catNameReader.run(Cat(catName, catFood)) == catName)
  }

  it can "be combined using map" in {
    assert(greetKittyReader.run(Cat(catName, catFood)) == "Hello Garfield")
  }

  it can "be combined using flatmap" in {
    assert(greetAndFeedReader.run(Cat(catName, catFood)) == "Hello Garfield. Have a nice bowl of Lasagne.")
  }
}