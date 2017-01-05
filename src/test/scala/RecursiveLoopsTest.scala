import org.scalatest._

class RecursiveLoopsTest extends FreeSpec with ShouldMatchers {

  "while Loop" in {
    def whileLoop(condition: => Boolean)(command: => Unit): Unit = {
      if (condition) {
        command
        whileLoop(condition)(command)
      } else ()
    }

    var x = 0
    whileLoop(x < 10) {
      x = x + 1
    }
    x shouldEqual 10
  }

  "for loop" in {
    def forLoop[T](state: T, condition: T => Boolean, update: T => T)(body: => Unit): Unit = {
      if (condition(state)) {
        body
        forLoop(update(state), condition, update)(body)
      }
    }

    var x = 0
    forLoop[String]("", _ !== "AAAAAA",  _ + "A") {
      x = x + 1
    }
    x shouldEqual 6
  }
}