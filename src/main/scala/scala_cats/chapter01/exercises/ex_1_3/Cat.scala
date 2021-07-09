package scala_cats.chapter01.exercises.ex_1_3

import PrintableInstances._

final case class Cat(name: String, age: Int, colour: String)

object Cat {
  // Automatically in scope as it is in companion object

   implicit val printableCat: Printable[Cat] = (c: Cat) => {
     val name = Printable.format(c.name)
     val age = Printable.format(c.age)
     val colour = Printable.format(c.colour)

     s"$name is a $age year-old $colour cat"
   }
}
