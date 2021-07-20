package scala_cats.chapter03.ex_3_6_1_1

object  ContravariantFunctor {
  trait Printable[A] { self =>
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] =
      (value: B) => self.format(func(value))
  }

  def format[A](a: A)(implicit p: Printable[A]) : String = p.format(a)

  val stringPrintable: Printable[String] = (value: String) => s"'$value'"

  val booleanPrintable: Printable[Boolean] = (value: Boolean) => if (value) "yes" else "no"
}
