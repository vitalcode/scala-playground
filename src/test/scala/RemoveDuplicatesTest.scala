import org.scalatest._

class RemoveDuplicatesTest extends FreeSpec with ShouldMatchers {

  def removeDuplicates[A](xs: List[A]): List[A] = {
    if (xs.isEmpty) xs
    else
      xs.head :: removeDuplicates(
        xs.tail filter (x => x != xs.head)
      )
  }

  "Remove duplicates" in {
    val listWithDuplicates = List(1, 2, 3, 4, 4, 5, 2, 2, 1, 1, 1, 1, 6)

    removeDuplicates(listWithDuplicates) should equal(
      List(1, 2, 3, 4, 5, 6)
    )
  }
}