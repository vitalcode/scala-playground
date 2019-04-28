import org.scalatest._
import cats._
import cats.data._
import cats.implicits._

class CatsMonad extends FreeSpec with Matchers {

  type Birds = Int

  case class Pole(left: Birds, right: Birds) {
    def landLeft(n: Birds): Pole = copy(left = left + n)
    def landRight(n: Birds): Pole = copy(right = right + n)
  }


  case class PoleM(left: Birds, right: Birds) {
    def landLeft(n: Birds): Option[PoleM] =
      if (math.abs((left + n) - right) < 4) copy(left = left + n).some
      else none[PoleM]
    def landRight(n: Birds): Option[PoleM] =
      if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
      else none[PoleM]
    def banana: Option[PoleM] = none[PoleM]
  }

  "Simple chain" in {
    Pole(0, 0).landLeft(1).landRight(1).landLeft(2) shouldBe Pole(3,1)

    Pole(0, 0).landLeft(1).landRight(4).landLeft(-1).landRight(-2) shouldBe Pole(0,2)
  }


  "Monadic chain" in {
    val r11 = Monad[Option].pure(PoleM(0, 0)) >>= {_.landRight(2)} >>= {_.landLeft(2)} >>= {_.landRight(2)}
    r11 shouldBe Some(PoleM(2,4))

    val r12 = for {
      p1 <- Monad[Option].pure(PoleM(0, 0))
      p2 <- p1.landRight(2)
      p3 <- p2.landLeft(2)
      p4 <- p3.landRight(2)
    } yield p4
    r12 shouldBe Some(PoleM(2,4))

    val r21 = Monad[Option].pure(PoleM(0, 0)) >>= {_.landLeft(1)} >>= {_.landRight(4)} >>= {_.landLeft(-1)} >>= {_.landRight(-2)}
    r21 shouldBe None

    val r22 = for {
      p1 <- Monad[Option].pure(PoleM(0, 0))
      p2 <- p1.landLeft(1)
      p3 <- p2.landRight(4)
      p4 <- p3.landLeft(-1)
      p5 <- p4.landRight(-2)
    } yield p5
    r21 shouldBe None

    val r31 = Monad[Option].pure(PoleM(0, 0)) >>= {_.landLeft(1)} >>= {_.banana} >>= {_.landRight(1)}
    r31 shouldBe None

    def r32: Option[PoleM] =
      for {
        p1 <- Monad[Option].pure(PoleM(0, 0))
        p2 <- p1.landLeft(2)
        _ <- none[PoleM]
        p3 <- p2.landRight(1)
      } yield p3
    r32 shouldBe None
  }
}
