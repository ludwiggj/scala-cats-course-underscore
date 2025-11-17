package scala_cats.casestudies.testing.take1

import scala_cats.UnitSpec
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class UptimeServiceSpec extends UnitSpec {
  "uptime service" can "return total uptime" in {
    val hosts: Map[String, Int] = Map("host1" -> 10, "host2" -> 6)
    val client: TestUptimeClient = new TestUptimeClient(hosts)
    val service: UptimeService = new UptimeService(client)
    val actual: Future[Int] = service.getTotalUptime(hosts.keys.toList)
    val expected: Int = hosts.values.sum

    // scala.concurrent.Future[Int] and Int are unrelated: they will most likely never compare equal
    // assert(actual == expected)

    // Can adapt the test to fit the asynchronous nature of code
    assert(Await.result(actual, 1.second) == expected)
  }
}