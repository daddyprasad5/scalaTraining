package DeckOfCards;
import scala.util.Random



  //clumsy enumeration definition

  sealed abstract class Suite
  case object Spade extends Suite
  case object Heart extends Suite
  case object Club extends Suite
  case object Diamond extends Suite

  sealed abstract class Rank
  case object Two extends Rank
  case object Three extends Rank
  case object Four extends Rank
  case object Five extends Rank
  case object Six extends Rank
  case object Seven extends Rank
  case object Eight extends Rank
  case object Nine extends Rank
  case object Ten extends Rank
  case object Jack extends Rank
  case object Queen extends Rank
  case object King extends Rank
  case object Ace extends Rank

  sealed abstract class PlayerType
  case object Pone extends PlayerType
  case object Dealer extends PlayerType


  //the interesting part
  case class Card(rank: Rank, suite: Suite)



  class Deck(inCards: List[Card] = List()) {

    val suites = Set(Spade, Heart, Club, Diamond)
    val ranks = List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)

    val pCards = {
      if (inCards.isEmpty)
        for (r <- ranks; s <- suites) yield Card(r, s)
      else inCards
    }

    val cards = if (isValidDeck(pCards)) pCards
    else throw new RuntimeException("Deck is invalid!")

    def shuffle() = new Deck(Random.shuffle(cards))

    def pullFromTop() = (cards.head, new Deck(cards.tail))

    def addToTop(card: Card) = new Deck(card :: cards)

    def addToTop(cardsToAdd: List[Card]) = new Deck(cardsToAdd ::: cards)

    private def isValidDeck(cards: List[Card]) = cards.size <= 52 && cards.distinct.size == cards.size

    def getCards = pCards

    def getCribbageValues(game: String, rank: Rank): Int = {
      if (game == "cribbage") rank match {
        case Two => 2
        case Three => 3
        case Four => 4
        case Five => 5
        case Six => 6
        case Seven => 7
        case Eight => 8
        case Nine => 9
        case Ten => 10
        case Jack => 10
        case Queen => 10
        case King => 10
        case Ace => 1
      }
      else throw new RuntimeException("I don't know that game")
    }

  }

  object Deck{
    def apply(inCards: List[Card] = List()): Deck = {
      new Deck(inCards)
    }
  }

object CardEncoder {
  private val cCode = Map(
    Card(Ace, Diamond) -> 1,
    Card(Two, Diamond) -> 2,
    Card(Three, Diamond) -> 3,
    Card(Four, Diamond) -> 4,
    Card(Five, Diamond) -> 5,
    Card(Six, Diamond) -> 6,
    Card(Seven, Diamond) -> 7,
    Card(Eight, Diamond) -> 8,
    Card(Nine, Diamond) -> 9,
    Card(Ten, Diamond) -> 10,
    Card(Jack, Diamond) -> 11,
    Card(Queen, Diamond) -> 12,
    Card(King, Diamond) -> 13,
    Card(Ace, Heart) -> 101,
    Card(Two, Heart) -> 102,
    Card(Three, Heart) -> 103,
    Card(Four, Heart) -> 104,
    Card(Five, Heart) -> 105,
    Card(Six, Heart) -> 106,
    Card(Seven, Heart) -> 107,
    Card(Eight, Heart) -> 108,
    Card(Nine, Heart) -> 109,
    Card(Ten, Heart) -> 110,
    Card(Jack, Heart) -> 111,
    Card(Queen, Heart) -> 112,
    Card(King, Heart) -> 113,
    Card(Ace, Club) -> 201,
    Card(Two, Club) -> 202,
    Card(Three, Club) -> 203,
    Card(Four, Club) -> 204,
    Card(Five, Club) -> 205,
    Card(Six, Club) -> 206,
    Card(Seven, Club) -> 207,
    Card(Eight, Club) -> 208,
    Card(Nine, Club) -> 209,
    Card(Ten, Club) -> 210,
    Card(Jack, Club) -> 211,
    Card(Queen, Club) -> 212,
    Card(King, Club) -> 213,
    Card(Ace, Spade) -> 301,
    Card(Two, Spade) -> 302,
    Card(Three, Spade) -> 303,
    Card(Four, Spade) -> 304,
    Card(Five, Spade) -> 305,
    Card(Six, Spade) -> 306,
    Card(Seven, Spade) -> 307,
    Card(Eight, Spade) -> 308,
    Card(Nine, Spade) -> 309,
    Card(Ten, Spade) -> 310,
    Card(Jack, Spade) -> 311,
    Card(Queen, Spade) -> 312,
    Card(King, Spade) -> 313
  )


  private val reverseCardCode = {
    for ((k,v) <- cCode) yield (v, k)
  }


  def cardsEncode (c: Card): Int = {
    cCode(c)
  }

  def cardsDecode (i: Int): Card = {
    reverseCardCode(i)
  }

  def cardsEncode (cs: Seq[Card]): Seq[Int] = {
    val a = for (c <- cs) yield this.cCode(c)
    a
  }

  def cardsDecode (is: Seq[Int]): Seq[Card] = {
    for (i <- is) yield reverseCardCode(i)
  }

  def pad(s: Seq[Int], d: Int): Seq[Int] = {
    val cnt = d - s.size
    val nulls: Seq[Int] = for (i <- 1 to cnt) yield 0
    s ++ nulls
  }

  def encodeHand(h: CribbageHand): Seq[Int] = {
    pad(cardsEncode(h.getCards.toSeq).sorted,4)
  }

  def encodePile(p: Pile): Seq[Int] = {
    pad(cardsEncode(p.getCards),8)
  }

  def encodeCrib(c: Crib, player: Int): Seq[Int] = {
    pad(cardsEncode(c.getCards(player)), 2)
  }
}



