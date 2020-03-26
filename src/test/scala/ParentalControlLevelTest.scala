//import org.scalatest.{Matchers, WordSpec}
//
//class ParentalControlLevelTest extends WordSpec with Matchers {
//
//  import ParentalControlLevel._
//
//  "ParentalControlLevel" when {
//    "comparing identical parental control level" should {
//      "return 0" in {
//        ParentalControlLevel.compare(PC_U, PC_U) shouldBe 0
//        ParentalControlLevel.compare(PC_PG, PC_PG) shouldBe 0
//        ParentalControlLevel.compare(PC_12, PC_12) shouldBe 0
//        ParentalControlLevel.compare(PC_15, PC_15) shouldBe 0
//        ParentalControlLevel.compare(PC_18, PC_18) shouldBe 0
//      }
//    }
//    "the first parental control level is less restrictive" should {
//      "return -1" in {
//        ParentalControlLevel.compare(PC_U, PC_PG) shouldBe -1
//        ParentalControlLevel.compare(PC_PG, PC_12) shouldBe -1
//        ParentalControlLevel.compare(PC_12, PC_15) shouldBe -1
//        ParentalControlLevel.compare(PC_15, PC_18) shouldBe -1
//        ParentalControlLevel.compare(PC_U, PC_18) shouldBe -1
//      }
//    }
//    "the first parental control level is more restrictive" should {
//      "return 1" in {
//        ParentalControlLevel.compare(PC_PG, PC_U) shouldBe 1
//        ParentalControlLevel.compare(PC_12, PC_PG) shouldBe 1
//        ParentalControlLevel.compare(PC_15, PC_12) shouldBe 1
//        ParentalControlLevel.compare(PC_18, PC_15) shouldBe 1
//        ParentalControlLevel.compare(PC_18, PC_U) shouldBe 1
//      }
//    }
//  }
//}
