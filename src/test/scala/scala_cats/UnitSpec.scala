package scala_cats

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._

import cats.tests.StrictCatsEquality

// See https://stackoverflow.com/questions/47501997/cats-eqs-with-scalatest
abstract class UnitSpec extends AnyFlatSpec with should.Matchers with StrictCatsEquality