package scala_cats.chapter05.ex_5_4

import cats.data.EitherT

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Autobot {
  // type Response[A] = Future[Either[String, A]]

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  // If an Autobot isn’t in the powerLevels map, return an error message reporting
  // that they were unreachable. Include the name in the message for good effect.
  def getPowerLevel(autobot: String): Response[Int] = {
    EitherT(Future(powerLevels.get(autobot).toRight(s"Could not reach $autobot")))
  }

  def getPowerLevel2(ally: String): Response[Int] = {
    powerLevels.get(ally) match {
      case Some(powerLevel) => EitherT.right(Future(powerLevel))
      case None => EitherT.left(Future(s"Could not reach $ally"))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      power1 <- getPowerLevel(ally1)
      power2 <- getPowerLevel(ally2)
    } yield power1 + power2 > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
      case Right(canSpecialMove) => if
      (canSpecialMove)
        s"$ally1 and $ally2 are ready to roll out!"
      else
        s"$ally1 and $ally2 need a recharge."

      case Left(errorString) => s"Comms error: $errorString"
    }

  }
}