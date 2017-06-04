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



