package DeckOfCards

/**
  * Created by btw460 on 5/29/17.
  */
class CribbageHand (cards: Set[Card]) {
  require( isValidHand(cards), "Not a valid Hand!" )

  private def isValidHand(cards: Set[Card]): Boolean = {
    cards.size <= 6
  }

  def getCards: Set[Card] = cards

  def addCard(c: Card) = new CribbageHand (getCards + c)

  def takeCard(c: Card) = new CribbageHand(getCards - c)

  def removeCard(card: Card): CribbageHand = {
    require(cards.contains(card))
    new CribbageHand(cards - card)
  }
}

object CribbageHand {
  def apply(cards: Set[Card] = Set()) = {
    new CribbageHand(cards)
  }
}