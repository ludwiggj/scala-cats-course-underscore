package scala_cats.casestudies.testing.take1

import scala.concurrent.Future

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient {
  def getUptime(hostname: String): Future[Int] =
    Future.successful(hosts.getOrElse(hostname, 0))
}
