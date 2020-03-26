//package play
//
//import org.scalatest._
//
//
//sealed trait MyOption[+A] extends Product with Serializable
//
//case class MySome[+A](fa: A) extends MyOption[A]
//
//case object MyNone extends MyOption[Nothing]
//
//trait MyMonad[M[_]] {
//  def pure[A](p: A): M[A]
//
//  def map[A, B](m: M[A])(f: A => B): M[B]
//
//  def flatMap[A, B](m: M[B])(f: A => M[B]): M[B]
//}
//
//object MyMonad {
//  def apply[M[_]](implicit instance: MyMonad[M]): MyMonad[M] = instance
//
//  trait Ops[M[_], A] {
//    def typeClassInstance: MyMonad[M]
//
//    def self: M[A]
//
//    def flatMap[B](f: A => M[B]): M[B] = typeClassInstance.flatMap(self)(f)
//
//    def map[B](f: A => B): M[B] = typeClassInstance.map(self)(f)
//  }
//
//
//trait MyMonadExtension[M[_], A] {
//
//  def monadTypeClass = MyMonad[M]
//
//  def monad: M[A]
//
//  def map[B](f: A => B): M[B] = monadTypeClass.map(monad)(f)
//}
//
//object MyMonad {
//  def apply[M[_]](implicit instance: MyMonad[M[_]]): MyMonad[M[_]]  = instance
//
//  implicit def myMonadExtension[M[_], A](m: MyMonad[M[_]]) = new MyMonadExtension[M[_]] {
//    override def monad = m
//  }
//}
//
//class CashPocketTest extends FreeSpec with Matchers {
//
//  implicit val myOptionMonad = new MyMonad[MyOption] {
//    override def pure[A](p: A) = ???
//
//    override def map[A, B](m: MyOption[A])(f: A => B) = ???
//
//    override def flatMap[A, B](m: MyOption[B])(f: A => MyOption[B]) = ???
//  }
//
//
//  "MyOption" - {
//    "both options has values" in {
//      val result = for {
//        value1 <- MyMonad[MyOption].pure(2)
//        value2 <- MyMonad[MyOption].pure(1)
//      } yield value1 - value2
//
//      result shouldBe MySome(1)
//    }
//  }
//}
//
