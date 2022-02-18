package scala_cats.casestudies.mapreduce

import cats.instances.int._
import cats.instances.string._
import scala_cats.UnitSpec

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt // for Monoid

class MapReduceSpec extends UnitSpec {
  val maxRange = 10000
  val rangeSum = 50005000

  // Taken from http://biercoff.com/easily-measuring-code-execution-time-in-scala/
  def time[R](identifier: String, block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(s"Elapsed time ($identifier) " + (t1 - t0)/1000 + " us")
    result
  }

  "foldMap" can "add numbers without changing them" in {
    assert(MapReduce.foldMap(Vector(1, 2, 3))(identity) == 6)
  }

  it can "map numbers to strings and then combine them" in {
    assert(MapReduce.foldMap(Vector(1, 2, 3))(_.toString + "! ") == "1! 2! 3! ")
  }

  it can "capitalise a string" in {
    assert(MapReduce.foldMap("Hello world!".toVector)(_.toString.toUpperCase) == "HELLO WORLD!")
  }

  it can "add long list of numbers" in {
    assert(time("foldMap", MapReduce.foldMap((1 to maxRange).toVector)(identity) == rangeSum))
  }

  "parallelFoldMap" can "add long list of numbers" in {
    assert(
      time(
        "parallelFoldMap1",
        Await.result(MapReduce.parallelFoldMap1((1 to maxRange).toVector)(identity), 1.second) == rangeSum
      )
    )
  }

  it can "add a second long list of numbers" in {
    assert(
      time(
        "parallelFoldMap2",
        Await.result(MapReduce.parallelFoldMap2((1 to maxRange).toVector)(identity), 1.second) == rangeSum
      )
    )
  }

  it can "add a third long list of numbers" in {
    assert(
      time(
        "parallelFoldMap3",
        Await.result(MapReduce.parallelFoldMap3((1 to maxRange).toVector)(identity), 1.second) == rangeSum
      )
    )
  }

  it can "add a long list of numbers cats stylee" in {
    assert(
      time(
        "parallelFoldMapMoreCats",
        Await.result(MapReduce.parallelFoldMapMoreCats((1 to maxRange).toVector)(identity), 1.second) == rangeSum
      )
    )
  }

  it can "add a long list of numbers cats textbook stylee" in {
    assert(
      time(
        "parallelFoldMapMoreCatsTextbook",
        Await.result(MapReduce.parallelFoldMapMoreCatsTextbook((1 to maxRange).toVector)(identity), 1.second) == rangeSum
      )
    )
  }
}
