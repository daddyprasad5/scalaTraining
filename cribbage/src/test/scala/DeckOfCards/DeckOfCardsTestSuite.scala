package DeckOfCards

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class deckTest extends FunSuite {
  test("cribbage hand should deal two hands of six different cards each "){

    val c = CribbageMaster.dealCribbageHands()

    assert(c.hand1.getCards.size === 6)
    assert(c.hand2.getCards.size === 6)
    assert((c.hand1.getCards ++ c.hand2.getCards).size === 12)
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

  test("creating a crib should work") {
    val cc = CribbageMaster.dealCribbageHands()
    assert(cc.crib.getCards.size == 0)
    val cc2 = CribbageMaster.makeCrib(cc)
    assert(cc2.crib.getCards.size == 4, "crib has wrong number of cards")
    assert(cc2.hand1.getCards.size == 4, "hand 1 has wrong number of cards")
    assert(cc2.hand2.getCards.size == 4, "hand 2 has wrong number of cards")
    assert(cc2.deck.getCards.size == 40, "hand 3 has wrong number of cards")
  }

  test("pegging should work") {

    val cc = CribbageMaster.makeCrib(CribbageMaster.dealCribbageHands())
    assert(cc.pile.getCards.size == 0)
    val p1StartingCards = cc.hand1.getCards
    val p2StartingCards = cc.hand2.getCards
    val cc2 = CribbageMaster.playPegging(cc)
    assert(cc2.pile.getCards.size == 8, "pile didn't have 4 cards")
    assert(cc2.hand1.getCards.isEmpty, "hand1 didn't have 0 cards")
    assert(cc2.hand2.getCards.isEmpty, "hand2 didn't have 0 cards")

    val pileCards = cc2.pile.getCards.toArray
    assert(pileCards.toSet == (p1StartingCards ++ p2StartingCards).toSet)

    val p1FromPile =Set(pileCards(0), pileCards(2), pileCards(4), pileCards(6))
    val p2FromPile =Set(pileCards(1), pileCards(3), pileCards(5), pileCards(7))

    assert(p1FromPile == p1StartingCards, "player 1 cards aren't showing up in the right places in the pile")
    assert(p2FromPile == p2StartingCards, "player 2 cards aren't showing up in the right places in the pile")

  }

  test("scorePegging should work") {
    //test that a number is returned
    val pile = Pile(Seq(Card(Ace, Heart)))
    val score = CribbageMaster.scorePegging(pile)
    val retInt = score match {
      case x: Int => true
      case _ => false
    }
    assert(retInt, "did not return int")

    //test that the whole pegging process works

    val cc = CribbageMaster.makeCrib(CribbageMaster.dealCribbageHands())
    assert(true)  //just testing that it finishes without error.

    //test that 15s are scored properly

    val fifteenPile2 = Pile(
      Seq(
        Card(Jack, Heart),
        Card(Five, Diamond),
        Card(Two, Spade)
      )
    )

    val fifteenPile3 = Pile(
      Seq(
        Card(Seven, Heart),
        Card(Four, Diamond),
        Card(Four, Heart)
      )
    )

    val fifteenPile5 = Pile(
      Seq(
        Card(Ace, Heart),
        Card(Five, Diamond),
        Card(Three, Heart),
        Card(Two, Club),
        Card(Four, Heart)
      )
    )

    val notFifteenPile1 = Pile(
      Seq(
        Card(Ace, Heart),
        Card(Five, Diamond),
        Card(Three, Heart),
        Card(Two, Club),
        Card(Six, Heart)
      )
    )

    val notFifteenPile2 = Pile(
      Seq(
        Card(Nine, Heart),
        Card(Seven, Diamond),
        Card(Three, Heart),
        Card(Two, Club),
        Card(Six, Heart)
      )
    )


    assert(CribbageMaster.scorePegging(fifteenPile2) == 2, "2 card fifteen didn't score properly")
    assert(CribbageMaster.scorePegging(fifteenPile3) == 2, "3 card fifteen didn't score properly")
    assert(CribbageMaster.scorePegging(fifteenPile5) == 2, "5 card fifteen didn't score properly")
    assert(CribbageMaster.scorePegging(notFifteenPile1) == 0, "not fifteen pile1 didn't score properly")
    assert(CribbageMaster.scorePegging(notFifteenPile2) == 0, "not fifteen pile2 didn't score properly")


    //test that pairs are scored properly
    val pairPile = Pile(
      Seq(
        Card(Ace, Heart),
        Card(Ace, Diamond),
        Card(Two, Spade)
      )
    )

    val pairScore = CribbageMaster.scorePegging(pairPile)
    val scorePair = pairScore match {
      case 2 => true
      case _ => false
    }
    assert(scorePair, "pair not Scored correctly")



    //test that triples are scored properly
    val triplePile = Pile(
      Seq(
        Card(Ace, Club),
        Card(Ace, Heart),
        Card(Ace, Diamond),
        Card(Two, Spade)
      )
    )

    val tripleScore = CribbageMaster.scorePegging(triplePile)
    val scoreTriple = tripleScore match {
      case 6 => true
      case _ => false
    }

    assert(scoreTriple, "triple not Scored correctly")

    //test that four-of-a-kind is scored properly

    val quadPile = Pile(
      Seq(
        Card(Ace, Spade),
        Card(Ace, Club),
        Card(Ace, Heart),
        Card(Ace, Diamond),
        Card(Two, Spade)
      )
    )

    val quadScore = CribbageMaster.scorePegging(quadPile)
    val scoreQuad = quadScore match {
      case 12 => true
      case _ => false
    }

    assert(scoreQuad, "four-of-a-kind not Scored correctly")

    //test that runs of three are scored correctly at upper edge

    val threeRunPile = Pile(
      Seq(
        Card(Jack, Spade),
        Card(Queen, Spade),
        Card(King, Spade),
        Card(Ace, Spade),
        Card(Two, Spade)
      )
    )

    val threeRunScore = CribbageMaster.scorePegging(threeRunPile)

    assert(threeRunScore == 3)

    //test that runs of two are not scored correctly

    val twoRunPile = Pile(
      Seq(
        Card(Queen, Spade),
        Card(King, Spade),
        Card(Ace, Spade),
        Card(Two, Spade)
      )
    )

    val twoRunScore = CribbageMaster.scorePegging(twoRunPile)

    assert(twoRunScore == 0)

    //test that runs of four are scored, and don't include wrong suit
    val fiveRunPile = Pile(
      Seq(
        Card(Eight, Spade),
        Card(Nine, Spade),
        Card(Ten, Spade),
        Card(Jack, Spade),
        Card(Queen, Heart)
      )
    )

    val fiveRunScore = CribbageMaster.scorePegging(fiveRunPile)
    assert(fiveRunScore == 4)

  }
}