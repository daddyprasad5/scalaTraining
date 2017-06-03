package DeckOfCards

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import DeckOfCards._

@RunWith(classOf[JUnitRunner])
class deckTest extends FunSuite {
  test("cribbage hand should deal two hands of six different cards each "){

    val (dealerHand, poneHand, deck) = CribbageMaster("cribMaster").dealCribbageHands()

    assert(dealerHand.getCards.size === 6)
    assert(poneHand.getCards.size === 6)
    assert((dealerHand.getCards ++ poneHand.getCards).size === 12)
  }

  test("adding to a Pile should work") {
    val expected: Pile = Pile(Seq(
      Card(Two,Heart),
      Card(Five,Heart),
      Card(Ace,Spade)
    ))

    val actual = new Pile()
      .add(Card(Ace,Spade))
      .add(Card(Five,Heart))
      .add(Card(Two,Heart))

    assert(expected.getCards === actual.getCards)
  }

  test("Player adding to a pile should work"){

    val (dealerHand, poneHand, deck) = CribbageMaster("cribMaster").dealCribbageHands()

  }

}