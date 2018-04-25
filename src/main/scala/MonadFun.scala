class MonadFun {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def pure[A](x: A): F[A]
  }

  object Monad {

    implicit object ListMonad extends Monad[List] {
      def map[A, B](fa: List[A])(f: A => B) = fa map f

      def flatMap[A, B](fa: List[A])(f: A => List[B]) = fa flatMap f

      def pure[A](x: A) = x :: Nil
    }

    implicit object OptionMonad extends Monad[Option] {
      def map[A, B](fa: Option[A])(f: A => B) = fa map f

      def flatMap[A, B](fa: Option[A])(f: A => Option[B]) = fa flatMap f

      def pure[A](x: A) = Some(x)
    }

    def apply[F[_] : Monad]: Monad[F] = implicitly[Monad[F]]

  }


  case class MonadOps[F[_], A](fa: F[A])(implicit monad: Monad[F]) {
    def map[B](f: A => B) = MonadOps(monad.map(fa)(f))

    def flatMap[B](f: A => MonadOps[F, B]) = MonadOps(monad.flatMap(fa)(a => f(a).fa))
  }


  for {
    d <- MonadOps(List(1, 2, 3))
    f <- MonadOps(List(1, 2, 3))
  } yield d + "+" + f

  for {
    d <- MonadOps(Option(1))
    f <- MonadOps(Option(2))
  } yield d + "+" + f
}
