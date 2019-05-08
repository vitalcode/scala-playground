import org.scalatest.{FreeSpec, Matchers}

class PatternMatchingTest extends FreeSpec with Matchers {

  "Case Classes and Pattern Matching" in {

    trait Operation
    sealed case class Value(v: Int) extends Operation
    sealed case class Addition(o1: Operation, o2: Operation) extends Operation
    sealed case class Subtraction(o1: Operation, o2: Operation) extends Operation

    def calculate(o: Operation): Int = o match {
      case Value(v) => v
      case Addition(o1, o2) => calculate(o1) + calculate(o2)
      case Subtraction(o1, o2) => calculate(o1) - calculate(o2)
    }

    val expression = Addition(
      Value(3),
      Subtraction(
        Value(10), Value(6)
      )
    )

    calculate(expression) shouldEqual 7
  }
}
