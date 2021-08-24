package scala_cats.chapter04

import cats.data.State

object StateMonad {
  val s = State[Int, String] { state =>
    (state, s"The state is $state")
  }

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String]{ num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both: State[Int, (String, String)] = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val program: State[Int, (Int, Int, Int)] = for {
    a <- State.get[Int]
    _ <- State.set[Int](a + 1)
    b <- State.get[Int]
    _ <- State.modify[Int](_ + 1)
    c <- State.inspect[Int, Int](_ * 1000)
  } yield (a, b, c)
}