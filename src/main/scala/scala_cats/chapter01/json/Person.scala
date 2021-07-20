package scala_cats.chapter01.json

import cats.Eq
import cats.syntax.eq._

case class Person(name: String, email: String)

object Person {
  implicit def eqPerson: Eq[Person] = Eq.instance[Person] { (p1, p2) =>
    p1.name === p2.name && p1.email === p2.email
  }
}