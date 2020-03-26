import scala.language.higherKinds
import cats.Id
import cats.Applicative
import cats.syntax.functor._
import cats.instances.future._ // for Applicative
import cats.instances.list._ // for Traverse
import cats.syntax.traverse._ // for traverse
import org.scalatest.{FreeSpec, Matchers}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

class CatsTestAsynchronousCode extends FreeSpec with Matchers {

  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

//  class RealUptimeClient extends UptimeClient[Future] {
//    def getUptime(hostname: String): Future[Int]
//  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
    def getUptime(hostname: String): Int =
      hosts.getOrElse(hostname, 0)
  }

  class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] = {
      hostnames.traverse(client.getUptime).map(_.sum)
    }
  }

  "Should be able to ..." in {

    def testTotalUptime() = {
      val hosts = Map("host1" -> 10, "host2" -> 6)
      val client = new TestUptimeClient(hosts)
      val service = new UptimeService(client)
      val actual = service.getTotalUptime(hosts.keys.toList)
      val expected = hosts.values.sum
      assert(actual == expected)
    }
  }
}
