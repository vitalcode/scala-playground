import org.scalatest._

class ForComprehensionTest extends FreeSpec with Matchers {


  "For Comprehension transformations" - {
    "with filter and map" in {

      case class Foo(a: String, b: String)

      val foos = Seq(
        Foo("a1", "bread"),
        Foo("a2", "bread"),
        Foo("a3", "bread"),
        Foo("a4", "butter"),
        Foo("a5", "butter"),
        Foo("a6", "butter")
      )

      val result = for {
        foo <- foos if foo.b == "butter"
      } yield foo.a

      result should equal(
        foos.filter(_.b == "butter").map(_.a)
      )
    }

    "with flat map" in {

      case class Foo(as: Seq[String], b: String)

      val foos = Seq(
        Foo(Seq("a11", "a12", "a13"), "bread"),
        Foo(Seq("a21", "a22", "a23"), "bread"),
        Foo(Seq("a31", "a32", "a33"), "bread"),
        Foo(Seq("a41", "a42", "a43"), "butter"),
        Foo(Seq("a51", "a52", "a53"), "butter"),
        Foo(Seq("a61", "a62", "a63"), "butter")
      )

      val result = for {
        foo <- foos
        a <- foo.as if a contains "3"
      } yield foo.b

      result should equal(
        foos.flatMap(foo => foo.as.filter(_.contains("3")).map(_ => foo.b))
      )
    }

    "with options" in {

      val optA: Option[String] = Some("a value")
      val optB: Option[String] = Some("b value")

      val result = for {
        a <- optA
        b <- optB
      } yield (a, b)

      result should equal(
        optA.flatMap(a => optB.map(b => (a, b)))
      )
    }

    "with options collection" in {

      val numbers = List(Some(1), Some(2), None, Some(3))

      val result = for {
        optNumber <- numbers
        value <- optNumber
      } yield value + 1

      result should equal(
        numbers.flatMap(number => number.map(value => value + 1))
      )
    }

    "Translating foldRight to a for Loop" in {

      def flatten[A](xss: List[List[A]]): List[A] =
        for {
          xs <- xss
          x <- xs
        } yield x

      val xss = List(List(1, 2), List(3, 4))

      flatten(xss) shouldEqual xss.flatten
      flatten(xss) shouldEqual xss.flatMap(identity)
    }
  }


  abstract class C[A] {
    def map[B](f: A => B): C[B]

    def flatMap[B](f: A => C[B]): C[B]

    def filter(p: A => Boolean): C[A]

    def foreach(b: A => Unit): Unit
  }

}