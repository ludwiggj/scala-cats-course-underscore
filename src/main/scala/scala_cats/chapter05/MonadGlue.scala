package scala_cats.chapter05

object MonadGlue {

  import cats.data.Writer

  type Logged[A] = Writer[List[String], A]

  // Methods generally return untransformed stacks:
  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None => Writer(List(s"Failed on $str"), None)
    }

  // Consumers use monad transformers locally to simplify composition:
  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT
    val result: OptionT[Logged, Int] = for {
      // final case class OptionT[F[_], A]        (value: F[Option[A]])
      //                  OptionT[Logged[_], Int] (value: Logged[Option[Int]])
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c
    result.value
  }

  // This approach doesn't force OptionT on other users' code
}