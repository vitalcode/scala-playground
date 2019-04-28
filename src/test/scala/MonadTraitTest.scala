import org.scalatest._


sealed trait MyOption[+A] extends Product with Serializable

case class MySome[+A](fa: A) extends MyOption[A]

case object MyNone extends MyOption[Nothing]

trait MyMonad[M[_]] {
  def pure[A](x: A): M[A]

  def map[A, B](ma: M[A])(f: A => B): M[B]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
}

object MyMonad {
  def apply[M[_]](implicit instance: MyMonad[M]): MyMonad[M] = instance

  trait Ops[M[_], A] {
    def typeClassInstance: MyMonad[M]

    def self: M[A]

    def flatMap[B](f: A => M[B]): M[B] = typeClassInstance.flatMap(self)(f)

    def map[B](f: A => B): M[B] = typeClassInstance.map(self)(f)
  }

  implicit def toMyMonadOps[M[_], A](target: M[A])(implicit tc: MyMonad[M]): Ops[M, A] = new Ops[M, A] {
    val self = target
    val typeClassInstance = tc
  }

  implicit val OptionMonad = new MyMonad[MyOption] {

    override def pure[A](x: A) = MySome(x)

    override def map[A, B](ma: MyOption[A])(f: A => B) = ma match {
      case MySome(fa) => pure(f(fa))
      case MyNone => MyNone
    }

    override def flatMap[A, B](ma: MyOption[A])(f: A => MyOption[B]) = ma match {
      case MySome(fa) => f(fa)
      case MyNone => MyNone
    }
  }
}

class CashPocketTest extends FreeSpec with Matchers {

  import MyMonad._

  "MyOption" - {
    "both options has values" in {
      val result = for {
        value1 <- MyMonad[MyOption].pure(2)
        value2 <- MyMonad[MyOption].pure(1)
      } yield value1 - value2

      result shouldBe MySome(1)
    }
  }
}

