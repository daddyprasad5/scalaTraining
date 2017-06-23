package DeckOfCards

/**
  * Created by btw460 on 5/29/17.
  */
class CribbageHand(cards: Seq[Card]) {
  require( isValidHand(cards), "Not a valid Hand!" )

  private def isValidHand(cards: Seq[Card]): Boolean = {
    cards.size <= 6
  }

  def getCards: Seq[Card] = cards

  def addCard(c: Card) = new CribbageHand(getCards :+ c)

  def takeCard(card: Card): CribbageHand = {
    require(cards.contains(card))
    new CribbageHand((cards.toSet - card).toSeq)
  }
}

object CribbageHand {
  def apply(cards: Seq[Card] = Seq()) = {
    new CribbageHand(cards)
  }
}


class Crib (p1cards: Seq[Card], p2cards: Seq[Card]) {

  require( isValidCrib(p1cards, p2cards), "Not a valid Hand!" )

  private def isValidCrib(p1cs: Seq[Card], p2s: Seq[Card]): Boolean = {
    p1cards.size <= 2 && p2cards.size <= 2
  }

  def getCards: Seq[Card] = p1cards ++ p2cards

  def getCards(player: Int = 0) = {
    player match {
      case 1 => p1cards
      case 2 => p2cards
      case _ => p1cards ++ p2cards
    }

  }

}

object Crib {
  def apply(p1cards: Seq[Card] = Seq(), p2cards: Seq[Card] = Seq()) = {
    new Crib(p1cards, p2cards)
  }
}
