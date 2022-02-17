package scala_cats.casestudies.testing.take1

import scala.concurrent.Future

// Modelled as a trait so it can be stubbed in unit tests
trait UptimeClient {
  def getUptime(hostname: String): Future[Int]
}
