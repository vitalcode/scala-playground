import org.scalatest._

class OptionTest extends FreeSpec with ShouldMatchers {

  sealed trait FunOption[+T] {

    def isEmpty: Boolean
  }

  case object FunNone extends FunOption[Nothing] {
    override def isEmpty: Boolean = true
  }

  case class FunSome[+T](value: T) extends FunOption[T] {
    def map[A](fn: T => A): FunSome[A] = {
      FunSome(fn(value))
    }

    def flatMap[A](fn: T => FunSome[A]): FunSome[A] = {
      fn(value)
    }

    def filter(fn: T => Boolean): FunOption[T] = {
      if (fn(value)) FunSome(value)
      else FunNone
    }

    override def isEmpty: Boolean = false
  }

  "FunSome" - {
    "map" in {
      FunSome(1).map(_.toString) shouldEqual FunSome("1")
    }
    "flatMap" in {
      FunSome(FunSome(1)).flatMap(identity) shouldEqual FunSome(1)
    }
    "filter" in {
      FunSome(1).filter(e => true) shouldEqual FunSome(1)
      FunSome(1).filter(e => false) shouldEqual FunNone
    }
    "for comprehension" in {
      val actual = for {
        e <- FunSome(1)
      } yield e

      actual shouldEqual FunSome(1)
    }
    "for comprehension with FunOption collection" in {
      val actual = for {
        e <- Seq(FunSome(1), FunSome(2), FunNone, FunSome(3))
        if !e.isEmpty
      } yield e

      actual shouldEqual Seq(FunSome(1), FunSome(2), FunSome(3))
    }
   // "covariant test"
  }
}

