import org.scalatest.{FreeSpec, Matchers}

class TupleList extends FreeSpec with Matchers {

  val t = (1, (2, ("tree", ("four", "last"))))

  "Should be able to iterate over tuple list" in {

    def print: PartialFunction[Any, Unit] = {
      case (head, tail) =>
        println(head)
        print(tail)
      case _ =>
    }

    print(t)
  }
}
