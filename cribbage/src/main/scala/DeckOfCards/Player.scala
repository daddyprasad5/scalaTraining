package DeckOfCards

class Player(name: String) {

  def playPeg(myHand: Hand, pile: Pile): Card = {
    //someday maybe this would be intelligent for an automated player
    //or would allow a human player to make a choice
    //but for now it just throws in the top card

    if (myHand.getCards.nonEmpty) myHand.getCards.head
    else throw new RuntimeException("I don't have any cards!")
  }

  def playToCrib(myHand: Hand): Seq[Card] = {
    //someday maybe this would be intelligent for an automated player
    //or would allow a human player to make a choice
    //but for now it just throws in the top cards
    require(myHand.getCards.size == 6, "My hand doesn't have the right number of cards!")
    val myCards = myHand.getCards
    Seq(myCards.head, myCards.tail.head)
  }

}

object Player {
  def apply(name: String): Player = {
    var p = new Player("name")
    p
  }
}