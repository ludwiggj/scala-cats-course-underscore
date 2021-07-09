package scala_cats.chapter01

import cats.Show
import cats.implicits.toShow

import java.util.Date

object CatsWorkout {
  object DateShow1 {
    implicit val dateShow: Show[Date] = (date: Date) => s"${date.getTime} ms since the epoch"
  }

  object DateShow2 {
    implicit val dateShow: Show[Date] = Show.show(date => s"${date.getTime} ms since the epoch")
  }

  def eqWorkout(): Unit = {
    import cats.syntax.eq._
    import cats.instances.int._

    assert(25 === 25)

    import cats.instances.option._

    assert(! ((Some(1): Option[Int]) === (None: Option[Int])))
    assert(! (Option(1) === Option.empty[Int]))

    import cats.syntax.option._

    assert(! (1.some === none[Int]))
    assert(1.some =!= none[Int])

    // Comparing custom types
    import java.util.Date
    import cats.instances.long._
    import cats.Eq

    implicit val dateEq: Eq[Date] = Eq.instance[Date] { (date1, date2) =>
      date1.getTime === date2.getTime
    }

    val x = new Date()
    Thread.sleep(5)
    val y = new Date()

    assert(! (x === y))
    assert(x =!= y)
  }

  def main(args: Array[String]): Unit = {
    {
      import DateShow1.dateShow
      println(new Date().show)
    }
    {
      import DateShow2.dateShow
      println(new Date().show)
    }

    import cats.instances.int._
    import cats.instances.string._

    println(21.show)
    println("Jump Street".show)

    eqWorkout()
  }
}
