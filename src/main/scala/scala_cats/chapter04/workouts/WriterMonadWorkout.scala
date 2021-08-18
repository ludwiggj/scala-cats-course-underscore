package scala_cats.chapter04.workouts

import cats.data.{Writer, WriterT}
import cats.{Id, data}
import scala_cats.chapter04.WriterMonad.Logged

object WriterMonadWorkout {
  
  // type Writer[L, V] = WriterT[Id, L, V]
  def main(args: Array[String]): Unit = {
    val wm1: WriterT[Id, Vector[String], Int] = Writer(Vector(
      "It was the best of times",
      "It was the worst of times"
    ), 1859)

    println(wm1.run)

    // Result only
    import cats.syntax.applicative._ // for pure
    val wm2: Logged[Int] = 123.pure[Logged]

    println(wm2.run)

    // Log only
    import cats.syntax.writer._ // for tell
    val wm3: data.Writer[Vector[String], Unit] = Vector("msg1", "msg2", "msg3").tell

    println(wm3.run)

    // Result and log
    val wm4: data.Writer[Vector[String], Int] = Writer(Vector("msg1", "msg2", "msg3"), 123)

    println(wm4.run)

    val wm5 = 123.writer(Vector("msg1", "msg2", "msg3"))

    println(wm5.run)
  }
}
