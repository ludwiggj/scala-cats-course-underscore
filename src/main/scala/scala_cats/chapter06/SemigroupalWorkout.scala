package scala_cats.chapter06

object SemigroupalWorkout {
  import cats.{Monoid, Semigroupal, Show}
  import cats.syntax.either._

  def parseInt(str: String): Either[String, Int] =
    Either.catchOnly[NumberFormatException](str.toInt).leftMap(_ => s"Couldn't read $str")

  def add3Numbers(a: String, b: String, c: String): Either[String, Int] = {
    for {
      parsedA <- parseInt(a)
      parsedB <- parseInt(b)
      parsedC <- parseInt(c)
    } yield parsedA + parsedB + parsedC
  }

  // contramap2 & imap2
  case class Dog(name: String, age: Int)

  implicit val semigroupalShow: Semigroupal[Show] = new Semigroupal[Show] {
    override def product[A, B](fa: Show[A], fb: Show[B]): Show[(A, B)] =
      Show.show(ab => s"${fa.show(ab._1)} ${fb.show(ab._2)}")
  }

  // contramap2
  // Create ability to show a dog from showing the component parts
  val showDog: Show[Dog] = Semigroupal.contramap2[Show, String, Int, Dog](Show[String], Show[Int]) {
    d => (d.name, d.age)
  }

  // imap2
  val anotherShowDog: Show[Dog] = Semigroupal.imap2[Show, String, Int, Dog](Show[String], Show[Int]) {
    (name, age) => Dog(name, age)
  } {
    d => (d.name, d.age)
  }

  // mapN
  case class Cat(name: String, born: Int, colour: String)

  val add: (Int, Int) => Int = (a, b) => a + b

  // Compile error (1)
  // type mismatch;
  // found   : (Int, Int) => Int
  // required: (Int, Int, Int) => ?

  // (Option(1), Option(2), Option(3)).mapN(add)

  // Compile error (2)
  // type mismatch;
  // found   : (Int, Int) => Int
  // required: (String, Boolean) => ?

  // (Option("cats"), Option(true)).mapN(add)

  object FancyApply {

    import cats.instances.invariant._ // for:

                                      // InvariantMonoidalInstances {
                                      //   implicit def catsSemigroupalForMonoid: InvariantSemigroupal[Monoid] = ...
                                      // }

                                      // trait InvariantSemigroupal[F[_]] extends Semigroupal[F] with Invariant[F]

                                      // This fulfils both implicits required by imapN method:

                                      // (implicit invariant: Invariant[F], semigroupal: Semigroupal[F])

    import cats.syntax.apply._ // for imapN

    final case class FancyCat(name: String, yearOfBirth: Int, favouriteFoods: List[String])

    val tupleToFancyCat: (String, Int, List[String]) => FancyCat = FancyCat.apply

    val fancyCatToTuple: FancyCat => (String, Int, List[String]) =
      cat => (cat.name, cat.yearOfBirth, cat.favouriteFoods)

    implicit val catMonoid: Monoid[FancyCat] = (
      Monoid[String],
      Monoid[Int],
      Monoid[List[String]]
      ).imapN(tupleToFancyCat)(fancyCatToTuple)
  }

  object Futures {
    // Semigroupal of future

    import scala.concurrent.ExecutionContext
    import scala.concurrent.Future
    import java.util.concurrent.Executors
    import cats.instances.future._

    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

    // NOTE - Futures start running before product is called
    val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))
  }
}
