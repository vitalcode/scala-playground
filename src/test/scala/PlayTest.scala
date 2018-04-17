import org.scalatest._

import scala.collection.SortedMap

class PlayTest extends FreeSpec with Matchers {

  sealed trait Instruction

  case object Update extends Instruction

  case object Delete extends Instruction

  case object New extends Instruction


  sealed trait Side

  case object Bid extends Side

  case object Ask extends Side


  case class ChangeOrderBookCommand(instruction: Instruction,
                                    side: Side,
                                    priceLevelIndex: Int,
                                    price: Int,
                                    quantity: Int)

  case class PriceLevel(price: BigDecimal, quantity: Int) {
    def withTickSize(tickSize: BigDecimal) = this.copy(price = price * tickSize)
  }


  case class OrderBook(bidPriceLevels: SortedMap[Int, PriceLevel],
                       askPriceLevels: SortedMap[Int, PriceLevel]) {

    def receive: PartialFunction[ChangeOrderBookCommand, OrderBook] = {
      case ChangeOrderBookCommand(New, Bid, index, price, quantity) =>
        this.copy(bidPriceLevels = bidPriceLevels + (index -> PriceLevel(price, quantity)))
      case ChangeOrderBookCommand(New, Ask, index, price, quantity) =>
        this.copy(askPriceLevels = askPriceLevels + (index -> PriceLevel(price, quantity)))
      case ChangeOrderBookCommand(Delete, Bid, index, price, quantity) =>
        this.copy(bidPriceLevels = bidPriceLevels - index)
      case ChangeOrderBookCommand(Delete, Ask, index, price, quantity) =>
        this.copy(askPriceLevels = askPriceLevels - index)
      case ChangeOrderBookCommand(Update, Bid, index, price, quantity) =>
        this.copy(bidPriceLevels = bidPriceLevels + (index -> PriceLevel(price, quantity)))
      case ChangeOrderBookCommand(Update, Ask, index, price, quantity) =>
        this.copy(askPriceLevels = askPriceLevels + (index -> PriceLevel(price, quantity)))
    }
  }

  object OrderBook {
    def createOrderBook(commands: Seq[ChangeOrderBookCommand], tickSize: BigDecimal, bookDepth: Int) = {
      def priceLevelsForbookDepth(priceLevels: SortedMap[Int, PriceLevel], bookDepth: Int) = {
        ((1 to bookDepth).foldLeft(SortedMap.empty[Int, PriceLevel]) { (levels, index) =>
          levels + (index -> PriceLevel(0, 0))
        } ++ priceLevels).take(bookDepth).mapValues(_.withTickSize(tickSize))
      }

      val book = commands.foldLeft(OrderBook(SortedMap.empty, SortedMap.empty)) { (book, command) =>
        book.receive(command)
      }

      book.copy(
        bidPriceLevels = priceLevelsForbookDepth(book.bidPriceLevels, bookDepth),
        askPriceLevels = priceLevelsForbookDepth(book.askPriceLevels, bookDepth)
      )
    }
  }


  "Market Update Instruction Types" - {
    val book = OrderBook(
      bidPriceLevels = SortedMap(
        1 -> PriceLevel(1, 10),
        3 -> PriceLevel(4, 40),
        4 -> PriceLevel(5, 50)
      ),
      askPriceLevels = SortedMap(
        1 -> PriceLevel(1, 10),
        3 -> PriceLevel(4, 40),
        4 -> PriceLevel(5, 50)
      )
    )
    "when insert a new price level" - {
      val bookWithNewBid = book.receive(ChangeOrderBookCommand(New, Bid, 2, 3, 30))
      val bookWithNewAsk = book.receive(ChangeOrderBookCommand(New, Ask, 2, 3, 30))
      "existing price levels with a greater or equal index should be shifted up" in {
        bookWithNewBid shouldBe book.copy(
          bidPriceLevels = SortedMap(
            1 -> PriceLevel(1, 10),
            2 -> PriceLevel(3, 30),
            3 -> PriceLevel(4, 40),
            4 -> PriceLevel(5, 50)
          ))

        bookWithNewAsk shouldBe book.copy(
          askPriceLevels = SortedMap(
            1 -> PriceLevel(1, 10),
            2 -> PriceLevel(3, 30),
            3 -> PriceLevel(4, 40),
            4 -> PriceLevel(5, 50)
          ))
      }
    }
    "when delete a price level" - {
      val bookWithDeletedBid = book.receive(ChangeOrderBookCommand(Delete, Bid, 1, 1, 10))
      val bookWithDeletedAsk = book.receive(ChangeOrderBookCommand(Delete, Ask, 1, 1, 10))
      "existing price levels with a higher index should be shifted down" in {
        bookWithDeletedBid shouldBe book.copy(
          bidPriceLevels = SortedMap(
            3 -> PriceLevel(4, 40),
            4 -> PriceLevel(5, 50)
          ))

        bookWithDeletedAsk shouldBe book.copy(
          askPriceLevels = SortedMap(
            3 -> PriceLevel(4, 40),
            4 -> PriceLevel(5, 50)
          ))
      }
    }
    "when update a price level" - {
      val bookWithUpdatedBid = book.receive(ChangeOrderBookCommand(Update, Bid, 4, 8, 80))
      val bookWithUpdatedAsk = book.receive(ChangeOrderBookCommand(Update, Ask, 4, 8, 80))
      "price level should contain the new values for an existing price level" in {
        bookWithUpdatedBid shouldBe book.copy(
          bidPriceLevels = SortedMap(
            1 -> PriceLevel(1, 10),
            3 -> PriceLevel(4, 40),
            4 -> PriceLevel(8, 80)
          ))

        bookWithUpdatedAsk shouldBe book.copy(
          askPriceLevels = SortedMap(
            1 -> PriceLevel(1, 10),
            3 -> PriceLevel(4, 40),
            4 -> PriceLevel(8, 80)
          ))
      }
    }
  }

  "Book Levels" - {
    "It is only necessary to keep track of given ‘Book Depth’ price levels" in {
      val book = OrderBook.createOrderBook(List(
        ChangeOrderBookCommand(New, Bid, 1, 1, 10),
        ChangeOrderBookCommand(New, Bid, 2, 2, 20),
        ChangeOrderBookCommand(New, Bid, 3, 3, 30),
        ChangeOrderBookCommand(New, Ask, 1, 1, 10),
        ChangeOrderBookCommand(New, Ask, 2, 2, 20),
        ChangeOrderBookCommand(New, Ask, 3, 3, 30)
      ), 1, 2)

      book shouldBe OrderBook(
        bidPriceLevels = SortedMap(
          1 -> PriceLevel(1, 10),
          2 -> PriceLevel(2, 20)
        ),
        askPriceLevels = SortedMap(
          1 -> PriceLevel(1, 10),
          2 -> PriceLevel(2, 20)
        )
      )
    }
    "Price levels within the range 1..book_depth that have not been provided should have values of zero" in {
      val book = OrderBook.createOrderBook(List(
        ChangeOrderBookCommand(New, Bid, 1, 1, 10),
        ChangeOrderBookCommand(New, Bid, 3, 3, 30)
      ), 1, 4)

      book shouldBe OrderBook(
        bidPriceLevels = SortedMap(
          1 -> PriceLevel(1, 10),
          2 -> PriceLevel(0, 0),
          3 -> PriceLevel(3, 30),
          4 -> PriceLevel(0, 0)
        ),
        askPriceLevels = SortedMap(
          1 -> PriceLevel(0, 0),
          2 -> PriceLevel(0, 0),
          3 -> PriceLevel(0, 0),
          4 -> PriceLevel(0, 0)
        )
      )
    }
  }

  "Example" - {
    val book = OrderBook.createOrderBook(List(
      ChangeOrderBookCommand(New, Bid, 1, 5, 30),
      ChangeOrderBookCommand(New, Bid, 2, 4, 40),
      ChangeOrderBookCommand(New, Ask, 1, 6, 10),
      ChangeOrderBookCommand(New, Ask, 2, 7, 10),
      ChangeOrderBookCommand(Update, Ask, 2, 7, 20),
      ChangeOrderBookCommand(Update, Bid, 1, 5, 40)
    ), 10.0, 2)

    book shouldBe OrderBook(
      bidPriceLevels = SortedMap(
        1 -> PriceLevel(50.0, 40),
        2 -> PriceLevel(40.0, 40)
      ),
      askPriceLevels = SortedMap(
        1 -> PriceLevel(60.0, 10),
        2 -> PriceLevel(70.0, 20)
      )
    )
  }
}