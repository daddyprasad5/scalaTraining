package DeckOfCards

/**
  * Created by btw460 on 5/29/17.
  */
class Hand(cards: Set[Card]) {
  require( isValidHand(cards), "Not a valid Hand!" )

  private def isValidHand(cards: Set[Card]): Boolean = {
    cards.size <= 6
  }

  def getCards: Set[Card] = cards

  def addCard(c: Card) = new Hand(getCards + c)

  def takeCard(c: Card) = new Hand(getCards - c)

  def removeCard(card: Card): Hand = {
    require(cards.contains(card))
    new Hand(cards - card)
  }
}

object Hand {
  def apply(cards: Set[Card]) = {
    new Hand(cards)
  }
}