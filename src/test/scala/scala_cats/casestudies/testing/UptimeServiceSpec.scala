package scala_cats.casestudies.testing

import cats.Id
import scala_cats.UnitSpec

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

class UptimeServiceSpec extends UnitSpec {
  "uptime service" can "return total uptime" in {
    val hosts: Map[String, Int] = Map("host1" -> 10, "host2" -> 6)
    val client = new take1.TestUptimeClient(hosts)
    val service = new take1.UptimeService(client)
    val actual: Future[Int] = service.getTotalUptime(hosts.keys.toList)
    val expected: Int = hosts.values.sum

    // scala.concurrent.Future[Int] and Int are unrelated: they will most likely never compare equal
    // assert(actual == expected)

    // Can adapt the test to fit the asynchronous nature of code
    assert(Await.result(actual, 1.second) == expected)
  }

  "new uptime service" can "return total uptime" in {
    val hosts: Map[String, Int] = Map("host1" -> 10, "host2" -> 6)
    val client = new take2.TestUptimeClient(hosts)
    val service = new take2.UptimeService(client)
    val actual: Id[Int] = service.getTotalUptime(hosts.keys.toList)
    val expected: Int = hosts.values.sum

    // Can now test with asynchronous nature of real code abstracted away
    assert(actual == expected)
  }
}