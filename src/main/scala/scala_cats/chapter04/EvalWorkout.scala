package scala_cats.chapter04

object EvalWorkout {
  def main(args: Array[String]): Unit = {
    // Call by value (eager and memoized)
    val x = {
      println("Computing x")
      math.random
    }
    println("Has x been computed yet?")
    println(x)
    println(x)

    // Call by name (lazy and not memoized)
    def y = {
      println("Computing y")
      math.random
    }

    println(y)
    println(y)

    // Call by need (lazy and memoized)
    lazy val z = {
      println("Computing z")
      math.random
    }

    println("Has z been computed yet?")
    println(z)
    println(z)

    import cats.Eval

    val now = Eval.now(math.random + 1000)
    println(now)

    val always = Eval.always(math.random + 3000)
    println(always)
    println(always.value)

    val later = Eval.later(math.random + 2000)
    println(later)
    println(later.value)

    // Eval.now (eager and memoized)
    val xx = Eval.now {
      println("Computing xx")
      math.random
    }

    println("Has xx been computed yet?")
    println(xx.value)
    println(xx.value)

    // Eval.always (lazy and not memoized)
    val yy = Eval.always {
      println("Computing yy")
      math.random
    }

    println(yy.value)
    println(yy.value)

    // Eval.later (lazy and memoized)
    val zz = Eval.later {
      println("Computing zz")
      math.random
    }

    println("Has zz been computed yet?")
    println(zz.value)
    println(zz.value)

    println("Defined greeting")

    val greeting = Eval
      .always {
        println("Step 1"); "Hello"
      }
      .map { str => println("Step 2"); s"$str world" }

    println("Wait for it!")

    println(greeting.value)

    println(greeting.value)

    println("Defined lazy greeting")

    // While the semantics of the originating Eval instances are maintained,
    // mapping functions are always called lazily on demand (def semantics):
    val lazyGreeting = Eval
      .later {
        println("Step 1"); "Hello"
      }
      .map { str => println("Step 2"); s"$str world" }

    println("Wait for it!")

    println(lazyGreeting.value)

    println(lazyGreeting.value)

    println("Defining ans")
    val ans = for {
      a <- Eval.now {
        println("Calculating A"); 40
      }
      b <- Eval.always {
        println("Calculating B"); 2
      }
    } yield {
      println("Adding A and B")
      a + b
    }
    println(s"ans is $ans")
    println(s"ans is ${ans.value}")
    println(s"ans is ${ans.value}")

    // Eval has a memoize method that allows us to memoize a chain of computations.
    // The result of the chain up to the call to memoize is cached, whereas
    // calculations after the call retain their original semantics:

    println("Memoizing part of the call chain")

    val saying = Eval
      .always {
        println("Step 1"); "The cat"
      }
      .map { str => println("Step 2"); s"$str sat on" }
      .memoize
      .map { str => println("Step 3"); s"$str the mat" }

    println(s"saying is $saying")
    println(s"saying is ${saying.value}")
    println(s"saying is ${saying.value}")

    def factorial(n: BigInt): BigInt =
      if (n == 1) n else n * factorial(n - 1)

    // println(s"Big factorial stack overflow! = ${factorial(50000).value}")

    // Rewrite the method using Eval to make it stack safe

    // BUT this still overflows, because we’re still making all the recursive calls to factorial
    // before we start working with Eval's map method.
    def factorialSafe1(n: BigInt): Eval[BigInt] =
      if (n == 1) {
        Eval.now(n)
      } else {
        factorialSafe1(n - 1).map(_ * n)
      }

    // println(s"Big factorial stack overflow! = ${factorialSafe1(50000).value}")

    def factorialSafe2(n: BigInt): Eval[BigInt] =
      if (n == 1) {
        Eval.now(n)
      } else {
        Eval.defer(factorialSafe2(n - 1).map(_ * n))
      }

    println(s"Big factorial finally = ${factorialSafe2(50000).value}")

  }
}