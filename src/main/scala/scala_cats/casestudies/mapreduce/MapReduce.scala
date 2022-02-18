package scala_cats.casestudies.mapreduce

import cats.{Foldable, Monoid, Traverse}
import cats.syntax.applicative._
import cats.syntax.monoid._
import cats.syntax.traverse._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

// https://toolsfun.com/math/sumrange
object MapReduce {

  def foldMap[A, B: Monoid](values: Vector[A])(f: A => B): B =
    values.map(f).foldLeft(Monoid[B].empty)(_ |+| _)

//  def parallelFoldMap[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
//    val groups: List[Vector[A]] = values.grouped(numberOfProcessors).toList
//
//    val parallelResults1: List[Future[B]] = groups.map(v => Future(v.map(f).foldLeft(Monoid[B].empty)(_ |+| _)))
//    val parallelResults2: List[Future[B]] = groups.map(v => v.map(f).foldLeft(Monoid[B].empty)(_ |+| _).pure[Future])
//    val parallelResults3: List[Future[B]] = groups.map(foldMap(_)(f).pure[Future])
//
//    parallelResults1.sequence.map(_.foldLeft(Monoid[B].empty)(_ |+| _))
//    parallelResults2.foldLeft(Monoid[B].empty.pure[Future])(Monoid[Future[B]].combine)
//    parallelResults3.foldLeft(Monoid[B].empty.pure[Future])(Monoid[Future[B]].combine)
//  }

  def groupSize[A](values: Vector[A]): Int = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    println(s"numCores [$numCores] total size [${values.size}] group size [$groupSize]")
    groupSize
  }

  // NOTE: This one is slower!
  def parallelFoldMap1[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val groups: Iterator[Vector[A]] = values.grouped(groupSize(values))

    val parallelResults1: Iterator[Future[B]] = groups.map { group =>
      Future {
        group.map(f).foldLeft(Monoid[B].empty)(_ |+| _)
      }
    }

    parallelResults1.toList.sequence.map(_.foldLeft(Monoid[B].empty)(_ |+| _))
  }

  def parallelFoldMap2[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val groups: List[Vector[A]] = values.grouped(groupSize(values)).toList

    val parallelResults2: List[Future[B]] = groups.map(group => group.map(f).foldLeft(Monoid[B].empty)(_ |+| _).pure[Future])

    parallelResults2.foldLeft(Monoid[B].empty.pure[Future])(Monoid[Future[B]].combine)
  }

  def parallelFoldMap3[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val groups: List[Vector[A]] = values.grouped(groupSize(values)).toList

    val parallelResults3: List[Future[B]] = groups.map(foldMap(_)(f).pure[Future])

    parallelResults3.foldLeft(Monoid[B].empty.pure[Future])(Monoid[Future[B]].combine)
  }

  def parallelFoldMapMoreCats[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
    Traverse[List]
      .sequence(values.grouped(groupSize(values)).map { group =>
        Future {
          Foldable[Vector].foldLeft(group.map(f), Monoid[B].empty)(_ |+| _)
        }
      }.toList)
      .map {
        Foldable[List].foldLeft(_, Monoid[B].empty)(_ |+| _)
      }
    }

  import cats.syntax.foldable._ // for foldMap and combineAll

  def parallelFoldMapMoreCatsTextbook[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] =
    values
      .grouped(groupSize(values))
      .toVector
      .traverse(group => Future(group.foldMap(f)))
      .map(_.combineAll)
}
