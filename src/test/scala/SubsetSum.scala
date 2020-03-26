import org.scalatest.{FreeSpec, Matchers}

import scala.collection.mutable.ListBuffer

class SubsetSum extends FreeSpec with Matchers {

  "Subset Sum Problem algorithms comparison" in {

    var result: ListBuffer[List[Int]] = ListBuffer()

    def recursiveSubset(list: Array[Int], sum: Int, index: Int = 0, current: Int = 0, accum: ListBuffer[Int]): Unit = {
      if (list.length < index || current > sum) return
      for (i <- index until list.length) {
        if (current + list(i) == sum) {
          result += list(i) :: accum.toList
        }
        recursiveSubset(list, sum, i + 1, current + list(i), accum += list(i))
        accum -= list(i)
      }
    }

    def recursiveSubsetNew(list: List[Int], sum: Int, agg: Int = 0, group: List[Int] = Nil, result: List[List[Int]] = Nil): List[List[Int]] = {
      if (agg > sum) return Nil
      list match {
        case h :: rest if agg + h == sum =>
          (h :: group) :: result ::: recursiveSubsetNew(rest, sum, agg, group, result)
        case h :: rest =>
          recursiveSubsetNew(rest, sum, agg + h, h :: group, result) ::: recursiveSubsetNew(rest, sum, agg, group, result)
        case _ => result
      }
    }

    def time[R](f: => R): R = {
      val t1 = System.nanoTime()
      val result = f
      println(s"time: ${(System.nanoTime - t1) / 1e6}ms")
      result
    }

    def test(numbers: Array[Int], sum: Int) = {
      val r1 = time {
        result = ListBuffer()
        recursiveSubset(numbers, sum, accum = new ListBuffer[Int]())
        result.toList
      }
      val r2 = time(recursiveSubsetNew(numbers.toList, sum))
      r1.map(_.sortBy(identity)) shouldBe r2.map(_.sortBy(identity))
      println()
    }

    val numbers1 = Array(1, 2, 3, 4, 6)
    val sum1 = 6

    val numbers2 = Array(-1, 1, 2, 3, 4, 6)
    val sum2 = 6

    val numbers3 = Array(47, 66, 94, 11, 69, 100, 85, 84, 93, 15
      , 2, 64, 62, 89, 22, 45, 72, 70, 88, 21, 35, 95, 78, 40, 43, 14
      , 32, 87, 27, 71, 8, 17, 75, 63, 29)
    val sum3 = 100

    val numbers4 = Array(47, 66, 94, 11, 69, 100, 85, 84, 93, 15
      , 2, 64, 62, 89, 22, 45, 72, 70, 88, 21, 35, 95, 78, 40, 43, 14
      , 32, 87, 27, 71, 8, 17, 75, 63, 29, 90, 54, 38, 82, 12, 77, 57
      , 50, 34, 97, 59, 24, 53, 26, 42, 99, 25, 51, 48, 80, 60, 81, 86
      , 1, 83, 5, 56, 74, 9, 7, 52, 68, 4, 61, 18, 91, 33, 10, 28, 92, 76
      , 96, 58, 46, 37, 3, 31, 55, 49, 6, 30, 65, 98, 23, 16, 73, 13, 67
      , 41, 44, 79, 39, 36, 19, 20)
    val sum4 = 100


    test(numbers1, sum1)
    test(numbers2, sum2)
    test(numbers3, sum3)
    test(numbers4, sum4)
  }
}
