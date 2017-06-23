package DeckOfCards


/**
  * Defines a pile of cards. e.g. the pegging pile in cribbage.
  *
  * @param cards Inital cards in the pile.  Defaults to empty.
  */
class Pile(cards: Seq[Card] = Seq()) {

  /**
    * Defines a pile of cards. e.g. the pegging pile in cribbage.
    *
    * @param c Card to add to the pile.
    * @return Pile with the new Card added to the "top" (first in the sequence)
    */
  def add(c: Card) = new Pile(c +: cards)
  def empty = new Pile()
  def init() = Pile(getCards.init)
  def tail() = Pile(getCards.tail)
  def last() = getCards.last
  def head() = getCards.head
  def getLast(numCards: Int): Seq[Card] = {
    def getLastIter(num: Int, pile: Seq[Card], cardsOut: Seq[Card]): Seq[Card] = {
      if (num == 0) cardsOut
      else {
        getLastIter(num - 1, cards.tail, cardsOut :+ cards.head)
      }
    }
    getLastIter(numCards, cards, Seq[Card]())
  }
  def getCards = cards
}

object Pile  {
  def apply(cards: Seq[Card] = Seq()): Pile = {
    var p = new Pile(cards)
    p
  }
}
