package scala_cats.chapter03

import org.scalatest.concurrent.ScalaFutures
import scala_cats.UnitSpec

class FunctorSpec extends UnitSpec with ScalaFutures {

  val func1: Int => Double = (x: Int) => x.toDouble
  val func2: Double => Double = _ * 2

  "functions" can "compose by hand" in {
    assert(func2(func1(1)) == 2.0)
  }

  it can "compose by andThen" in {
    assert((func1 andThen func2)(1) == 2.0)
  }

  "functor" can "map functions" in {
    import cats.instances.function._ // For functor
    import cats.syntax.functor._ // for map

    assert((func1 map func2)(1) == 2.0)

    val f1 = (a: Int) => a + 1
    val f2 = (a: Int) => a * 2
    val f3 = (a: Int) => s"$a!"
    val f4 = f1 map f2 map f3

    assert(f4(123) == "248!")
  }

  it can "map lists" in {
    import cats.Functor
    import cats.instances.list._
    assert(Functor[List].map(List(1, 2, 3))(_ * 2) == List(2, 4, 6))
  }

  it can "map options" in {
    import cats.Functor
    import cats.instances.option._
    assert(Functor[Option].map(Some(123))(_.toString) == Some("123"))
  }

  it can "lift a function" in {
    import cats.Functor
    import cats.instances.option._
    val func: Int => Int = (x: Int) => x + 1
    val liftedFunc: Option[Int] => Option[Int] = Functor[Option].lift(func)
    assert(liftedFunc(Option(1)) == Some(2))
  }

  it can "replace value" in {
    import cats.Functor
    import cats.instances.list._
    assert(Functor[List].as(List(1, 2, 3), "As") == List("As", "As", "As"))

    import cats.syntax.functor._
    assert(List(1, 2, 3).as("As") == List("As", "As", "As"))
  }

  import cats.Functor
  import cats.syntax.functor._

  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
    start.map(n => (n + 1) * 2)

  it can "do abstract maths" in {
    import cats.instances.option._

    assert(doMath(Option(20)) == Some(42))

    import cats.instances.list._

    assert(doMath(List(2, 4, 6))== List(6, 10, 14))
  }

  it can "add options manually" in {
    implicit val optionFunctor: Functor[Option] = new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
    }

    assert(doMath(Option(20)) == Some(42))
  }

  it can "add futures manually" in {
    import scala.concurrent.{Future, ExecutionContext}

    implicit def futureFunctor(implicit ec: ExecutionContext): Functor[Future] = new Functor[Future] {
      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa map f
    }

    // We write this:
    //
    // Functor[Future]
    //
    // The compiler expands to this first:
    //
    // Functor[Future](futureFunctor)
    //
    // And then to this:
    //
    // Functor[Future](futureFunctor(executionContext))

    import scala.concurrent.ExecutionContext.Implicits.global

    assert(doMath(Future(20)).futureValue == 42)
  }

  "tree functor" can "map a leaf" in {
    import TreeFunctor.treeFunctor

    assert(Functor[Tree].map(Leaf(2))(_ * 6) == Leaf(12))
    assert(Tree.leaf(2).map(_ * 6) == Leaf(12))
  }

  it can "map a branch" in {
    import TreeFunctor.treeFunctor
    val t1 = Branch(Branch(Leaf("a"), Branch(Leaf("b"), Leaf("c"))), Leaf("d"))
    val t2 = Tree.branch(Branch(Leaf("a"), Branch(Leaf("b"), Leaf("c"))), Leaf("d"))
    val expected = Branch(Branch(Leaf("A"), Branch(Leaf("B"), Leaf("C"))), Leaf("D"))
    
    assert(Functor[Tree].map(t1)(_.toUpperCase) == expected)
    assert(t2.map(_.toUpperCase) == expected)
  }
}