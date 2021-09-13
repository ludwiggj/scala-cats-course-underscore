package kleislis.take2

import cats.data.Kleisli
import cats.effect.IO
import kleislis.take2.Logger.CorrelationId

object LoggingWorkout {
  type EntityB = String
  type EntityBId = String

  case class EntityA(entityBId: EntityBId) {
    def idOfB: EntityBId = entityBId
  }

  trait ServiceA {
    def createA(a: EntityA): Kleisli[IO, CorrelationId, _]
  }

  trait ClientB {
    def getB(bId: EntityBId): Kleisli[IO, CorrelationId, EntityB]
  }

  private val clientB = new ClientB with Logger {
    override def getB(bId: EntityBId): Kleisli[IO, CorrelationId, EntityB] = {
      val value = s"($bId,EntityB)"
      for {
        result <- Kleisli.liftF[IO, String, EntityB](IO(value))  // Kleisli[IO, String, EntityB]
        _ <- info(s"getB [$bId] Result [$value]")          // Kleisli[IO, CorrelationId, Unit]
      } yield result
    }
  }

  case class ServiceC(clientB: ClientB) extends ServiceA with Logger {
    def somePrivateBusinessLogic(entityB: EntityB): Kleisli[IO, EntityB, EntityB] =
      for {
        result <- Kleisli.liftF[IO, EntityB, EntityB] (IO {                       // Kleisli[IO, EntityB, EntityB]
          val intermediate = entityB.split(",")
          s"(${intermediate(1)},${intermediate(0)})"
        })
        _ <- info(s"somePrivateBusinessLogic [$entityB] Result [$result]")  // Kleisli[IO, CorrelationId, Unit]
      } yield result

    override def createA(a: EntityA): Kleisli[IO, CorrelationId, EntityB] = {
      for {
        entityB <- clientB.getB(a.idOfB)                                      // Kleisli[IO, CorrelationId, EntityB]
        processed <- somePrivateBusinessLogic(entityB)                        // Kleisli[IO, EntityB, EntityB]
        // TODO Kleisli that has int as input?
        askResult <- Kleisli.ask[IO, CorrelationId]                           // Kleisli[IO, CorrelationId, CorrelationId]
        _ <- info(s"Ask Result [$askResult]")                           // Kleisli[IO, CorrelationId, Unit]
        _ <- info(s"createA entity [${a.idOfB}] Result [$processed]")   // Kleisli[IO, CorrelationId, Unit]
      } yield processed
    }
  }

  def main(args: Array[String]): Unit = {
    val k = ServiceC(clientB).createA(EntityA("991"))

    println(s"Result = ${k.run("XZUP-8890-1234-IOAW").unsafeRunSync()}")
  }
}