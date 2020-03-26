import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

class MockPlayTest extends FreeSpec with Matchers with MockFactory with ScalaFutures {

  trait Database {
    def getData: String
    def setData(data: String): String
  }

  trait Repository {
    def get: String
    def set(data: String): String
  }

  class DefaultRepository(database: Database) extends Repository {
    override def get = database.getData

    override def set(data: String) = database.setData(data)
  }

  "The repository" - {
    "should return correct data from the database" in {
      val database = mock[Database]
      (database.getData _).expects().returns("some data").repeat(2)

      val repository = new DefaultRepository(database)

      repository.get shouldBe "some data"
    }
    "usign fake db stub" in {
      val database = stub[Database]
      (database.getData _).when().returns("some other data")

      val repository = new DefaultRepository(database)

      repository.get shouldBe "some other data"
    }
  }

  "Let's try some futures tests" in {

    val p1 = Promise[Int]()


    val f1 = p1.future
    val f2 = Future(2)

    p1.success(1)
    whenReady(f1)(c => c shouldBe Option(1))
  }

}
