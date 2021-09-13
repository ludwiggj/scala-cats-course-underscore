package kleislis.take1

object Solution08 {
  def main(args: Array[String]): Unit = {

    val negate_1: Boolean => (Boolean, String) = { (x: Boolean) =>
      (!x, s"negate_1 called with $x\n")
    }

    val negate_2: Boolean => (Boolean, String) = { (x: Boolean) =>
      (!x, s"negate_2 called with $x\n")
    }

    // String appending happens inside the composition
    // This is combining results of 2 functions i.e. composing functions

    // Is associative
    def composeT[A](f: A => (A, String),
                    g: A => (A, String)): A => (A, String) = { (x: A) =>
      val p1 = f(x)
      val p2 = g(p1._1)
      (p2._1, p1._2 + p2._2)
    }

    // Identity function
    def id[A](a: A): (A, String) = (a, "")

    def composed: Boolean => (Boolean, String) = composeT(negate_1, negate_2)

    println(s"Result: ${composed.apply(true)}")

    println(s"Result: ${composeT(composed, id[Boolean]).apply(true)}")

    // At this point we know we have associativity and identity — in other words we have a Category.
    // In our Category the objects are Scala types and the arrows are A -> (B, String)
    // (instead of simple A -> B arrows). The fact that the arrows are not simple transformation
    // from A -> B makes them so called Kleisli arrows. Kleisli arrows can work with many different
    // types, not only a tuple of some type B and String, they are defined for the types we impose
    // as few conditions as possible, in other words for monadic types. This is where Cats
    // definition of Kleisli comes in.

    // Kleisli enables composition of functions that return a monadic value, for instance an
    // Option[Int] or a Either[String, List[Double]], without having functions take an Option or
    // Either as a parameter, which can be strange and unwieldy.
  }
}
