package scala_cats.casestudies.testing.take2

import cats.Id

import scala.concurrent.Future

// Modelled as a trait so it can be stubbed in unit tests
trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class RealUptimeClient extends UptimeClient[Future] {
  override def getUptime(hostname: String): Future[Int] = Future.successful(hostname match {
    case "host1" => 10
    case "host2" => 6
    case _ => 0
  })
}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  override def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0)
}