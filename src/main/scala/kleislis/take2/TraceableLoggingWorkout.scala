package kleislis.take2

import cats.effect.IO
import kleislis.take2.Entity.{EntityB, EntityBId}
import kleislis.take2.RIO.RIO

object TraceableLoggingWorkout {

  trait ServiceA {
    def createA(a: EntityA): RIO[_]
  }

  trait ClientB {
    def getB(bId: EntityBId): RIO[EntityB]
  }

  private val clientB = new ClientB with Logger {

  override def getB(bId: EntityBId): RIO[EntityB] = {
      val value = s"($bId,EntityB)"
      for {
        result <- RIO.liftF[EntityB](IO(value))
        _ <- info(s"getB [$bId] Result [$value]")
      } yield result
    }
  }

  case class ServiceC(clientB: ClientB) extends ServiceA with Logger {
    def somePrivateBusinessLogic(entityB: EntityB): RIO[EntityB] =
      for {
        result <- RIO.liftF[EntityB] (IO {
          val intermediate = entityB.split(",")
          s"(${intermediate(1)},${intermediate(0)})"
        })
        _ <- info(s"somePrivateBusinessLogic [$entityB] Result [$result]")
      } yield result

    override def createA(a: EntityA): RIO[EntityB] =
      for {
        entityB <- clientB.getB(a.idOfB)
        processed <- somePrivateBusinessLogic(entityB)
        askResult <- RIO.ask
        _ <- info(s"Ask Result [$askResult]")
        _ <- info(s"createA entity [${a.idOfB}] Result [$processed]")
      } yield processed
  }

  def main(args: Array[String]): Unit = {
    val k = ServiceC(clientB).createA(EntityA("991"))

    println(s"Result = ${k.run(CorrelationId("XZUP-8890-1234-IOAW")).unsafeRunSync()}")
  }
}