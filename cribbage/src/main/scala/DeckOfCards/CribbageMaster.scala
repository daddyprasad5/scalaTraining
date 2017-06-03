package DeckOfCards

class CribbageMaster(name: String) {

  def dealHand(numCards: Int = 6, deck: Deck = new Deck()): (Hand, Deck) = {

    def dealHand(hand: Hand, deck: Deck, count: Int): (Hand, Deck) = {
      if (count == 0) (hand, deck)
      else {
        val (newCard, nextDeck) = deck.pullFromTop()
        val nextHand = hand.addCard(newCard)
        dealHand(nextHand, nextDeck, count - 1)
      }
    }
    dealHand(new Hand(Set()), deck, numCards)
  }

  def dealCribbageHands(): (Hand, Hand, Deck) = {
    val (dealerHand, nextDeck1) = dealHand()
    val (poneHand, nextDeck2) = dealHand(deck = nextDeck1)
    (dealerHand, poneHand, nextDeck2)
  }

  def makeCrib(deck: Deck = Deck(), player1: Player = Player("player1"), player2: Player = Player("player2")):
    = (hand1: Hand, hand2: Hand,
  {

    val pile = player1.playToCrib(new Hand())

  }

}

object CribbageMaster {
  def apply(name: String): CribbageMaster = {
    new CribbageMaster("name")
  }
}
