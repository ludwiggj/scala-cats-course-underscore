package scala_cats.casestudies.testing.take2

import cats.Id
import cats.instances.future._
import scala_cats.UnitSpec
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class UptimeServiceSpec extends UnitSpec {
  "uptime service" can "return total uptime" in {
    val hosts: Map[String, Int] = Map("host1" -> 10, "host2" -> 6)
    val client: UptimeClient[Future] = new RealUptimeClient()
    val service: UptimeService[Future] = new UptimeService(client)
    val actual: Future[Int] = service.getTotalUptime(hosts.keys.toList)
    val expected: Int = hosts.values.sum

    assert(Await.result(actual, 1.second) == expected)
  }

  "new uptime service" can "return total uptime" in {
    val hosts: Map[String, Int] = Map("host1" -> 10, "host2" -> 6)
    val client: UptimeClient[Id] = new TestUptimeClient(hosts)
    val service: UptimeService[Id] = new UptimeService(client)
    val actual: Id[Int] = service.getTotalUptime(hosts.keys.toList)
    val expected: Int = hosts.values.sum

    // Can now test with asynchronous nature of real code abstracted away
    assert(actual == expected)
  }
}