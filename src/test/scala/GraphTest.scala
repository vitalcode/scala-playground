import org.scalatest._

import scala.annotation.tailrec

class GraphTest extends FreeSpec with Matchers {

  // https://codereview.stackexchange.com/questions/29699/bfs-and-dfs-in-scala

  "Play with graph" - {
    "should be able to do DFS (Depth-first search) and BFS (Breadth-first search)" in {

      class Graph[T] {
        type Vertex = T
        type GraphMap = Map[Vertex, List[Vertex]]
        var g: GraphMap = Map()

        def BFS(start: Vertex): List[List[Vertex]] = {

          @tailrec
          def BFS0(elems: List[Vertex], visited: List[List[Vertex]]): List[List[Vertex]] = {
            val newNeighbors = elems.flatMap(g(_)).filterNot(visited.flatten.contains).distinct
            if (newNeighbors.isEmpty)
              visited
            else
              BFS0(newNeighbors, newNeighbors :: visited)
          }

          BFS0(List(start), List(List(start))).reverse
        }

        def DFS(start: Vertex): List[Vertex] = {

          def DFS0(v: Vertex, visited: List[Vertex]): List[Vertex] = {
            if (visited.contains(v))
              visited
            else {
              val neighbours: List[Vertex] = g(v) filterNot visited.contains
              neighbours.foldLeft(v :: visited)((b, a) => DFS0(a, b))
            }
          }

          DFS0(start, List()).reverse
        }
      }


      val intGraph = new Graph[Int]

      intGraph.g = Map(1 -> List(2, 4), 2 -> List(1, 3), 3 -> List(2, 4), 4 -> List(1, 3))

      intGraph.BFS(1) shouldBe List(List(1), List(2, 4), List(3))

      intGraph.BFS(2) shouldBe List(List(2), List(1, 3), List(4))

      intGraph.DFS(3) shouldBe List(3, 2, 1, 4)

      var sGraph = new Graph[String]
      sGraph.g = Map("Apple" -> List ("Banana","Pear","Grape"), "Banana" -> List("Apple","Plum"), "Pear" -> List("Apple","Plum"), "Grape" -> List("Apple","Plum"), "Plum" -> List ("Banana","Pear","Grape"))

      sGraph.BFS("Apple") shouldBe List(List("Apple"), List("Banana", "Pear", "Grape"), List("Plum"))
    }
  }
}
