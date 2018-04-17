import org.scalatest.{FreeSpec, Matchers}

class StringCombinatorTest extends FreeSpec with Matchers {

  "Combine fixed number of strings" in {

    val str1 = "abc"
    val str2 = "def"
    val str3 = "ghi"

    val simpleCombinator = for {
      s1 <- str1
      s2 <- str2
      s3 <- str3
    } yield s"$s1$s2$s3"

    simpleCombinator shouldBe Seq(
      "adg", "adh", "adi", "aeg", "aeh", "aei", "afg", "afh", "afi",
      "bdg", "bdh", "bdi", "beg", "beh", "bei", "bfg", "bfh", "bfi",
      "cdg", "cdh", "cdi", "ceg", "ceh", "cei", "cfg", "cfh", "cfi"
    )


    def combinator(strs: List[List[Char]], acc: String): List[String] = {
      strs match {
        case (headIn :: Nil) :: Nil => List(acc + headIn)
        case (headIn :: Nil) :: tail => combinator(tail, acc + headIn)
        case (headIn :: tailIn) :: Nil => List(acc + headIn) ::: combinator(tailIn :: Nil, acc)
        case (headIn :: tailIn) :: tail => combinator(tail, acc + headIn) ::: combinator(tailIn :: tail, acc)
      }
    }


    val strs: List[List[Char]] = List(str1, str2, str3).map(_.toList)

    combinator(strs, "") shouldBe simpleCombinator
  }
}

