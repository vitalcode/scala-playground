import org.scalatest._

import scala.annotation.tailrec

class SortAlgorithmsTest extends FreeSpec with Matchers {

  "Insertion sort: O(n^2)" - {
    "in-place comparison-based sorting algorithm, a sub-list is maintained which is always sorted" in {

      def isort(xs: List[Int]): List[Int] =
        if (xs.isEmpty) Nil
        else insert(xs.head, isort(xs.tail))

      def insert(x: Int, xs: List[Int]): List[Int] =
        if (xs.isEmpty || x <= xs.head) x :: xs
        else xs.head :: insert(x, xs.tail)

      isort(List(1, 4, 7, 3, 4, 2, 8, 9, 32, 13, 4)) shouldEqual List(1, 2, 3, 4, 4, 4, 7, 8, 9, 13, 32)
    }
  }

  "Insertion sort 2: O(n^2)" in {
    //https://github.com/vkostyukov/scalacaster/blob/master/src/sort/InsertionSort.scala

    def isort[A](list: List[A])(implicit o: A => Ordered[A]): List[A] = {
      def sort(as: List[A], bs: List[A]): List[A] = as match {
        case h :: t => sort(t, insert(h, bs))
        case Nil => bs
      }

      def insert(a: A, as: List[A]): List[A] = as match {
        case h :: t if a > h => h :: insert(a, t)
        case _ => a :: as
      }

      sort(list, Nil)
    }

    isort(List(1, 4, 7, 3, 4, 2, 8, 9, 32, 13, 4)) shouldEqual List(1, 2, 3, 4, 4, 4, 7, 8, 9, 13, 32)
  }

  "Insertion sort 3: O(n^2)" in {

    @tailrec
    def inSort(list: List[Int], result: List[Int] = Nil): List[Int] = list match {
      case h :: tail =>
        inSort(tail, in(h, result))
      case Nil =>
        result
    }

    def in(e: Int, list: List[Int]): List[Int] = list match {
      case h :: tail if h < e =>
        h :: in(e, tail)
      case _ =>
        e :: list
    }

    inSort(List(1, 4, 7, 3, 4, 2, 8, 9, 32, 13, 4)) shouldEqual List(1, 2, 3, 4, 4, 4, 7, 8, 9, 13, 32)
  }

  "Selection sort: O(n^2)" in {
    // https://stackoverflow.com/questions/1672074/selection-sort-in-functional-scala

    def selectionSort(xs: List[Int]) = {
      def selectionSortHelper(xs: List[Int], accumulator: List[Int]): List[Int] =
        if (xs.isEmpty) accumulator
        else {
          val ys = maximum(xs)
          selectionSortHelper(ys.tail, ys.head :: accumulator)
        }

      selectionSortHelper(xs, Nil)
    }

    def maximum(xs: List[Int]): List[Int] =
      (List(xs.head) /: xs.tail) {
        (ys, x) =>
          if (x > ys.head) (x :: ys)
          else (ys.head :: x :: ys.tail)
      }

    selectionSort(List(1, 4, 7, 3, 4, 2, 8, 9, 32, 13, 4)) shouldEqual List(1, 2, 3, 4, 4, 4, 7, 8, 9, 13, 32)
  }

  "Selection sort 2: O(n^2)" in {
    // https://rosettacode.org/wiki/Sorting_algorithms/Selection_sort

    def selectionSort[T <% Ordered[T]](list: List[T]): List[T] = {
      def remove(e: T, list: List[T]): List[T] =
        list match {
          case Nil => Nil
          case x :: xs if x == e => xs
          case x :: xs => x :: remove(e, xs)
        }

      list match {
        case Nil => Nil
        case _ =>
          val min = list.min
          min :: selectionSort(remove(min, list))
      }
    }

    selectionSort(List(1, 4, 7, 3, 4, 2, 8, 9, 32, 13, 4)) shouldEqual List(1, 2, 3, 4, 4, 4, 7, 8, 9, 13, 32)
  }

  "Selection sort 3: O(n^2)" in {
    //https://github.com/vkostyukov/scalacaster/blob/master/src/sort/SelectionSort.scala

    def selectionSort[A <% Ordered[A]](list: List[A]): List[A] = {
      def sort(as: List[A], bs: List[A]): List[A] = as match {
        case h :: t => select(h, t, Nil, bs)
        case Nil => bs
      }

      def select(m: A, as: List[A], zs: List[A], bs: List[A]): List[A] =
        as match {
          case h :: t =>
            if (m > h) select(m, t, h :: zs, bs)
            else select(h, t, m :: zs, bs)
          case Nil => sort(zs, m :: bs)
        }

      sort(list, Nil)
    }

    selectionSort(List(1, 4, 7, 3, 4, 2, 8, 9, 32, 13, 4)) shouldEqual List(1, 2, 3, 4, 4, 4, 7, 8, 9, 13, 32)
  }

  "Selection sort 4: O(n^2)" in {
    @tailrec
    def selSort(list: List[Int], result: List[Int] = Nil): List[Int] = list match {
      case h :: tail =>
        val m = max(tail, h)
        selSort(remove(list, m), m :: result)
      case _ =>
        result
    }


    def remove(list: List[Int], e: Int): List[Int] = list match {
      case h :: tail if h == e =>
        tail
      case h :: tail =>
        h :: remove(tail, e)
      case Nil =>
        Nil

    }

    @tailrec
    def max(list: List[Int], current: Int): Int = list match {
      case h :: tail if h > current =>
        max(tail, h)
      case _ :: tail =>
        max(tail, current)
      case _ =>
        current
    }

    selSort(List(1, 4, 7, 3, 4, 2, 8, 9, 32, 13, 4)) shouldEqual List(1, 2, 3, 4, 4, 4, 7, 8, 9, 13, 32)
  }

  "Merge Sort Ο(n log n)" in {
    // http://wuciawe.github.io/scala/algorithm/2014/09/03/merge-sort-implementation-in-scala.html
    // https://www.tutorialspoint.com/data_structures_algorithms/merge_sort_algorithm.htm

    def msort(xs: List[Int]): List[Int] = {

      @tailrec
      def merge(res: List[Int], xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (_, Nil) =>
          res.reverse ::: xs
        case (Nil, _) =>
          res.reverse ::: ys
        case (x :: xs1, y :: ys1) =>
          if (x < y)
            merge(x :: res, xs1, ys)
          else
            merge(y :: res, xs, ys1)
      }

      val n = xs.length / 2
      if (n == 0) xs
      else {
        val (ys, zs) = xs splitAt n
        merge(Nil, msort(ys), msort(zs))
      }
    }

    msort(List(1, 4, 7, 3, 4, 2, 8, 9, 32, 13, 4)) shouldEqual List(1, 2, 3, 4, 4, 4, 7, 8, 9, 13, 32)
  }

  "Quick sort Ο(n log n) - Ο(n2)" in {
    // http://www.scala-lang.org/docu/files/ScalaByExample.pdf
    // https://www.khanacademy.org/computing/computer-science/algorithms/quick-sort/a/overview-of-quicksort
    // https://www.tutorialspoint.com/data_structures_algorithms/quick_sort_algorithm.htm

    def sort(xs: Array[Int]): Array[Int] = {
      if (xs.length <= 1) xs
      else {
        val pivot = xs(xs.length / 2)
        Array.concat(
          sort(xs filter (pivot >)),
          xs filter (pivot ==),
          sort(xs filter (pivot <)))
      }
    }

    sort(Array(1, 4, 7, 3, 4, 2, 8, 9, 32, 13, 4)) shouldEqual Array(1, 2, 3, 4, 4, 4, 7, 8, 9, 13, 32)
  }

  "Quick sort using Scala partition" in {
    // http://stackoverflow.com/questions/2314526/a-generic-quicksort-in-scala

    def qsort[T](list: List[Int]): List[Int] = {
      list match {
        case Nil => Nil
        case x :: xs =>
          val (before, after) = xs partition (_ < x)
          qsort(before) ::: (x :: qsort(after))
      }
    }

    qsort(List(1, 4, 7, 3, 4, 2, 8, 9, 32, 13, 4)) shouldEqual List(1, 2, 3, 4, 4, 4, 7, 8, 9, 13, 32)
  }


  // Bubble sort https://www.tutorialspoint.com/data_structures_algorithms/bubble_sort_algorithm.htm]]


  "Bubble sort using 2" in {
    // http://pawel-malczyk.pl/wordpress/?p=498

    def bubbleSort(input: List[Int]): List[Int] = {
      if (input != Nil && input.tail != Nil) {
        if (input.head > input.tail.head) {
          bubbleSort(List(input.tail.head, input.head) ::: input.tail.tail)
        } else {
          val sortResult = bubbleSort(input.tail)
          if (input.head > sortResult.head) bubbleSort(List(sortResult.head, input.head) ::: sortResult.tail) else List(input.head) ::: sortResult
        }
      } else {
        input
      }
    }

    bubbleSort(List(1, 4, 7, 3, 4, 2, 8, 9, 32, 13, 4)) shouldEqual List(1, 2, 3, 4, 4, 4, 7, 8, 9, 13, 32)
  }

  "Bubble sort Ο(n^2) using 5" in {
    // https://rosettacode.org/wiki/Sorting_algorithms/Bubble_sort#Scala

    def bubbleSort(xt: List[Int]) = {
      @tailrec
      def bubble(xs: List[Int], rest: List[Int], sorted: List[Int]): List[Int] = xs match {
        case x :: Nil =>
          if (rest.isEmpty) x :: sorted
          else bubble(rest, Nil, x :: sorted)
        case a :: b :: xs =>
          if (a > b) bubble(a :: xs, b :: rest, sorted)
          else bubble(b :: xs, a :: rest, sorted)
      }

      bubble(xt, Nil, Nil)
    }

    bubbleSort(List(1, 4, 7, 3, 4, 2, 8, 9, 32, 13, 4)) shouldEqual List(1, 2, 3, 4, 4, 4, 7, 8, 9, 13, 32)
  }


  // https://interactivepython.org/runestone/static/pythonds/SortSearch/TheBinarySearch.html
  // https://www.tutorialspoint.com/data_structures_algorithms/binary_search_algorithm.htm

  "Binary search Ο(log n)" in {
    // https://rosettacode.org/wiki/Binary_search#Scala

    def binarySearch[A <% Ordered[A]](a: IndexedSeq[A], v: A) = {
      def recurse(low: Int, high: Int): Option[Int] = (low + high) / 2 match {
        case _ if high < low => None
        case mid if a(mid) > v => recurse(low, mid - 1)
        case mid if a(mid) < v => recurse(mid + 1, high)
        case mid => Some(mid)
      }

      recurse(0, a.size - 1)
    }

    binarySearch(Vector(1, 4, 7, 3, 4, 2, 8, 9, 32, 13, 4), 32) shouldEqual Some(8) // index
  }

  "Linear search Ο(n)" in {
    // https://github.com/vkostyukov/scalacaster/blob/master/src/search/LinearSearch.scala

    def linearsearch[A](list: List[A], key: A): Option[A] = {
      def search(as: List[A]): Option[A] =
        if (as.isEmpty) None
        else if (as.head == key) Some(as.head)
        else search(as.tail)

      search(list)
    }

    linearsearch(List(1, 4, 7, 3, 4, 2, 8, 9, 32, 13, 4), 32) shouldEqual Some(32) // finds element
  }
}

// https://www.khanacademy.org/computing/computer-science/algorithms/merge-sort/a/divide-and-conquer-algorithms