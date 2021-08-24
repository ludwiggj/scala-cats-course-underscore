package scala_cats.chapter04.ex_4_9_3

import cats.data.State
import cats.implicits.catsSyntaxApplicativeId

import scala.util.{Failure, Success, Try}

// (1 + 2) * 3)

// Evaluated as:

// 1 2 + 3 * // see 1, push onto stack
// 2 + 3 *   // see 2, push onto stack
// + 3 *     // see +, pop 1 and 2 off of stack,
//           // push (1 + 2) = 3 in their place
// 3 3 *     // see 3, push onto stack
// 3 *       // see 3, push onto stack
// *         // see *, pop 3 and 3 off of stack,
//           // push (3 * 3) = 9 in their place
object PostOrderCalculator {
  // Let’s write an interpreter for these expressions. We can parse each symbol
  // into a State instance representing a transformation on the stack and an
  // intermediate result. The State instances can be threaded together using
  // flatMap to produce an interpreter for any sequence of symbols.

  // Start by writing a function evalOne that parses a single symbol into an
  // instance of State. Use the code below as a template. Don’t worry about
  // error handling for now — if the stack is in the wrong configuration, it’s
  // OK to throw an exception.

  // If this seems difficult, think about the basic form of the State instances
  // you’re returning. Each instance represents a functional transformation
  // from a stack to a pair of a stack and a result. You can ignore any wider
  // context and focus on just that one step:

  // State[List[Int], Int] { oldStack =>
  //   val newStack = someTransformation(oldStack)
  //   val result = someCalculation(newStack, result)
  // }
  type CalcState[A] = State[List[Int], A]

  //noinspection ZeroIndexToHead
  def evalOne(sym: String): CalcState[Int] = State[List[Int], Int] { state =>
    Try(sym.toInt) match {
      case Success(i) => (i +: state, i)
      case Failure(_) =>
        val result = sym match {
          case "+" => state(1) + state(0)
          case "-" => state(1) - state(0)
          case "*" => state(1) * state(0)
          case "/" => state(1) / state(0)
          case _ => throw new IllegalArgumentException(s"invalid symbol $sym")
        }
        (result +: (state drop 2), result)
    }
  }

  def evalOneTextbook(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case n => operand(n.toInt)
    }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)

      case _ =>
        sys.error("Fail!")
    }

  def evalAll(input: List[String]): CalcState[Int] =
    input.map(evalOne) match {
      case Nil => sys.error("Fail!")
      case x :: Nil => x
      case x :: xs => xs.fold(x) { case (a, b) =>
        for {
          _ <- a
          ans <- b
        } yield ans
      }
    }

  def evalAll2(input: List[String]): CalcState[Int] =
    input.map(evalOne) match {
      case Nil => sys.error("Fail!")
      case x :: Nil => x
      case x :: xs => xs.fold(x) { (a, b) =>
        a.flatMap(_ => b.map(identity))
      }
    }

  def evalAllTextbook(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOneTextbook(b))
    }

  def evalInput(input: String): Int =
    evalAllTextbook(input.split(" ").toList).runA(Nil).value
}