package scala_cats.chapter03

import scala_cats.UnitSpec
import scala_cats.chapter03.ex_3_6_1_1.Box
import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.{decode, encode}

class InvariantFunctorSpec extends UnitSpec {

  "int codec" can "encode an int" in {
    import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.Codecs.intCodec
    assert(encode(6) === "6")

    assert(intCodec.encode(6) === "6")
  }

  it can "decode a string" in {
    import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.Codecs.intCodec
    assert(decode("66") === 66)

    assert(intCodec.decode("66") === 66)
  }

  "boolean codec" can "encode a boolean" in {
    import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.Codecs.booleanCodec
    assert(encode(false) === "false")

    assert(booleanCodec.encode(false) === "false")
  }

  it can "decode a string" in {
    import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.Codecs.booleanCodec
    assert(decode("false") !== true)

    assert(booleanCodec.decode("false") !== true)
  }

  "double codec" can "encode an double" in {
    import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.Codecs.doubleCodec
    assert(encode(2.5d) === "2.5")

    assert(doubleCodec.encode(2.5d) === "2.5")
  }

  it can "decode a string" in {
    import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.Codecs.doubleCodec

    assert(decode[Double]("3.14d") === 3.14d)
    assert(decode("3.14d") === 3.14d)
    assert(doubleCodec.decode("3.14d") === 3.14d)
  }

  "box codec" can "encode a box" in {
    import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.Codecs.boxCodec

    import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.Codecs.intCodec
    assert(encode(Box(24)) === "24")

    import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.Codecs.booleanCodec
    assert(encode(Box(true)) === "true")

    import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.Codecs.doubleCodec
    assert(encode(Box(2.5d)) === "2.5")
  }

  it can "decode a string" in {
    import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.Codecs.boxCodec

    import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.Codecs.intCodec
    assert(decode[Box[Int]]("24") === Box(24))

    import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.Codecs.booleanCodec
    assert(decode[Box[Boolean]]("true") === Box(true))

    import scala_cats.chapter03.ex_3_6_2_1.InvariantFunctor.Codecs.doubleCodec
    assert(decode[Box[Double]]("2.5") === Box(2.5d))
  }

  // Imagine we want to produce a Monoid for Scala’s Symbol type. Cats doesn’t
  // provide a Monoid for Symbol but it does provide a Monoid for a similar type:
  // String.

  // We can write our new semigroup with an empty method that relies
  // on the empty String, and a combine method that works as follows:

  // 1. accept two Symbols as parameters;
  // 2. convert the Symbols to Strings;
  // 3. combine the Strings using Monoid[String];
  // 4. convert the result back to a Symbol.

  // We can implement combine using imap, passing functions of type String =>
  // Symbol and Symbol => String as parameters.

  "symbol monoid" can "combine symbols" in {
    // NOTE: Following imports suggested by page 70 don't work wrt finding imap method
    import cats.Monoid
    import cats.instances.string._ // for Monoid
    import cats.syntax.invariant._ // for imap
    import cats.syntax.semigroup._ // for |+|

    implicit val symbolMonoid: Monoid[Symbol] =
      Monoid[String].imap(Symbol.apply)(_.name)

    assert(Monoid[Symbol].empty.toString() === "'")

    assert((Symbol("a") |+| Symbol("few") |+| Symbol("words")).toString() === "'afewwords")
  }
}