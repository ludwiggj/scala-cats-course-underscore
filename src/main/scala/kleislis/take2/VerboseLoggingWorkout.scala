package kleislis.take2

import cats.data.Kleisli
import cats.effect.IO
import kleislis.take2.Entity.{EntityB, EntityBId}

object VerboseLoggingWorkout {

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
        result <- Kleisli.liftF[IO, CorrelationId, EntityB](IO(value))  // Kleisli[IO, CorrelationId, EntityB]
        _ <- info(s"getB [$bId] Result [$value]")                 // Kleisli[IO, CorrelationId, Unit]
      } yield result
    }
  }

  case class ServiceC(clientB: ClientB) extends ServiceA with Logger {
    def somePrivateBusinessLogic(entityB: EntityB): Kleisli[IO, CorrelationId, EntityB] =
      for {
        result <- Kleisli.liftF[IO, CorrelationId, EntityB] (IO {                 // Kleisli[IO, CorrelationId, EntityB]
          val intermediate = entityB.split(",")
          s"(${intermediate(1)},${intermediate(0)})"
        })
        _ <- info(s"somePrivateBusinessLogic [$entityB] Result [$result]")  // Kleisli[IO, CorrelationId, Unit]
      } yield result

    // def intKleisli: Kleisli[IO, Int, Int] =
    //  Kleisli.liftF[IO, Int, Int](IO(1))

    // def andThen[C](f: B => F[C])(implicit F: FlatMap[F]): Kleisli[F, A, C]
    // def andThen[C](f: Kleisli[F, B, C])(implicit F: FlatMap[F]): Kleisli[F, A, C]
    // def flatMap[C, AA <: A](f: B => Kleisli[F, AA, C])(implicit F: FlatMap[F]): Kleisli[F, AA, C]
    override def createA(a: EntityA): Kleisli[IO, CorrelationId, EntityB] =
      for {
        entityB <- clientB.getB(a.idOfB)                                      // Kleisli[IO, CorrelationId, EntityB]
        processed <- somePrivateBusinessLogic(entityB)                        // Kleisli[IO, CorrelationId, EntityB]
        // NOTE: Does not work with a Kleisli that has Int as input
        // _ <- intKleisli
        askResult <- Kleisli.ask[IO, CorrelationId]                           // Kleisli[IO, CorrelationId, CorrelationId]
        _ <- info(s"Ask Result [$askResult]")                           // Kleisli[IO, CorrelationId, Unit]
        _ <- info(s"createA entity [${a.idOfB}] Result [$processed]")   // Kleisli[IO, CorrelationId, Unit]
      } yield processed
  }

  //  Retrieving / generating the CorrelationId

  //  case class ApiA(serviceA: ServiceA) {
  //    def postA() = Route { request =>
  //      val correlationId = request.headers("X-Correlation-Id")
  //        .map(CorrelationId)
  //        .getOrElse(CorrelationId.new)
  //
  //      serviceA.createA(request.body.decodeJson[EntityA])
  //        .run(correlationId)
  //    }
  //  }

  //  Passing the CorrelationId to another service

  //  case class HttpClientB(httpClient: HttpClient) extends ClientB {
  //    def getB(bId: EntityBID): Klesli[IO, CorrelationId, EntityB] = Kleisli { cid =>
  //      httpClient
  //        .url(s"https://my-b-service-url.com/api/b-entity/bId")
  //        .headers("X-Correlation-Id" -> cid)
  //        .get()
  //    }
  //  }

  def main(args: Array[String]): Unit = {
    val k = ServiceC(clientB).createA(EntityA("991"))

    println(s"Result = ${k.run(CorrelationId("XZUP-8890-1234-IOAW")).unsafeRunSync()}")
  }
}