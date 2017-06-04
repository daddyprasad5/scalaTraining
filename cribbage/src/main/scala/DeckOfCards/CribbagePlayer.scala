package DeckOfCards

abstract class CribbagePlayer {

  def playPeg(c: PlayerCribbageContext): Card

  def playToCrib(c: PlayerCribbageContext): (Seq[Card] )

}

class AutoPlayer1(name: String) extends CribbagePlayer{

  def playPeg(c: PlayerCribbageContext): Card = {
    //someday maybe this would be intelligent for an automated player
    //or would allow a human player to make a choice
    //but for now it just throws in the top card

    if (c.hand.getCards.nonEmpty) c.hand.getCards.head
    else throw new RuntimeException("I don't have any cards!")
  }

  def playToCrib(c: PlayerCribbageContext): (Seq[Card] ) = {
    //someday maybe this would be intelligent for an automated player
    //or would allow a human player to make a choice
    //but for now it just throws in the top cards
    require(c.hand.getCards.size <= 6, "My hand doesn't have the right number of cards!")
    val c1 = c.hand.getCards.head
    val c2 = c.hand.getCards.tail.head
    Seq(c1, c2)
  }

}

object AutoPlayer1 {
  def apply(name: String): AutoPlayer1 = {
    new AutoPlayer1("name")
  }
}