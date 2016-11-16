import org.scalatest._
import TypeClass.Semigroup

class TypeClassTest extends WordSpec with ShouldMatchers {

  "Type class" when {
    "used to implement Semigroup for Int" should {
      implicit val intSemigroup = new Semigroup[Int] {
        def append(x: Int, y: Int): Int = x + y
      }
      "provide correct interface" in {
        intSemigroup.append(1, 2) shouldBe 3
        Semigroup[Int].append(1, 2) shouldBe 3

        import TypeClass.Semigroup.op._
        1 |+| 2 shouldBe 3
      }
    }
  }
}
