package DeckOfCards

/** The score of a two player cribbage game
  *
  *  @constructor use the companion object to create a new instance of the class.
  *  @param s the initial score, defaults to (0,0)
  */
class Score( s: (Int, Int)) {
  def add(addScore: (Int, Int)): Score = Score((s._1 + addScore._1, s._2 + addScore._2))
  def score = s
}

/** Factory for DeckOfCards.Score instances. */
object Score {
  def apply(score: (Int, Int) = (0,0)): Score = {
    new Score(score)
  }
}

/** The score of a two player cribbage game
  *
  *  @constructor use the companion object to create a new instance of the class.
  *  @param player1: the first player
  *  @param player2: the second player
  *  @param hand1: the first player's hand
  *  @param hand2: the second player's hand
  *  @param deck: the deck of cards
  *  @param score: the initial score
  *  @param dealer: the player who is the dealer in the next hand
  */
case class CribbageContext(
                            player1: CribbagePlayer = AutoPlayer1("player1"),
                            player2: CribbagePlayer = AutoPlayer1("player2"),
                            dealer: Int = 1,
                            hand1: CribbageHand = CribbageHand(),
                            hand2: CribbageHand = CribbageHand(),
                            crib: Crib = Crib(),
                            deck: Deck = Deck(),
                            pile: Pile = Pile(),
                            score: Score = Score())

case class PlayerCribbageContext (
                                   dealer: Boolean,
                                   hand: CribbageHand,
                                   crib: Crib,
                                   pile: Pile,
                                   score: Score
                                 )

/** The master of a two player cribbage game - runs the game!
  *
  *  @constructor use the companion object to create a new instance of the class.
  */
object CribbageMaster{


  def validateCribbageContext(c: CribbageContext): (Boolean, String) = {

    val numCards =
      c.deck.getCards.size +
      c.hand1.getCards.size +
      c.hand2.getCards.size +
      c.crib.getCards.size +
      c.pile.getCards.size


    val numCardsTest: (Boolean, String) = {
      if (numCards != 52) {
        (false, numCards.toString + " is not the right number of cards; ")
      }
      else (true, "")
    }

    numCardsTest
  }

  def playerCribbageContext(playerNum: Int, c: CribbageContext): PlayerCribbageContext = {
    val (dealer, hand) = {
      if (playerNum == 1)
        if (c.dealer == 1) (true, c.hand1)
        else (false, c.hand1)
      else { //player!= !=1
        if (c.dealer == 1) (false, c.hand2)
        else (true, c.hand2)
      }
    }
    PlayerCribbageContext(dealer, hand, c.crib, c.pile, c.score)
  }

  private def dealHand(numCards: Int = 6, deck: Deck = new Deck().shuffle()): (CribbageHand, Deck) = {

    def dealHand(hand: CribbageHand, deck: Deck, count: Int): (CribbageHand, Deck) = {
      if (count == 0) (hand, deck)
      else {
        val (newCard, nextDeck) = deck.pullFromTop()
        val nextHand = hand.addCard(newCard)
        dealHand(nextHand, nextDeck, count - 1)
      }
    }
    dealHand(CribbageHand(), deck, numCards)
  }

  def dealCribbageHands(): CribbageContext = {
    val c = CribbageContext()
    val (dealerHand, nextDeck1) = dealHand()
    val (poneHand, nextDeck2) = dealHand(deck = nextDeck1)
    val newContext = c.copy(hand1 = dealerHand, hand2 = poneHand, deck = nextDeck2)
    val testContext = validateCribbageContext(newContext)
    if (!testContext._1) throw new RuntimeException(testContext._2)
    else newContext
  }

  def takeCardsFromHand(hand: CribbageHand, cards: Seq[Card]): CribbageHand = {
    if (cards.isEmpty) hand
    else {
      takeCardsFromHand(hand.takeCard(cards.head), cards.tail)
    }
  }

  def makeCrib(c: CribbageContext): CribbageContext = {

    def validContext(c: CribbageContext): Boolean = {
      //hands should be full, crib should be empty, deck should have 40 cards
      if (c.crib.getCards.size == 6 &
        c.hand2.getCards.size == 6 &
        c.deck.getCards.size == 40
      ) false
      else true
    }

    if (!validContext(c)) {
      throw new RuntimeException("cribbage context is not valid for making a crib")
    }
    else {

      val p1Cards: Seq[Card] = c.player1.playToCrib(playerCribbageContext(1, c))
      val p2Cards: Seq[Card] = c.player2.playToCrib(playerCribbageContext(2, c))
      val p1Hand = takeCardsFromHand(c.hand1, p1Cards)
      val p2Hand = takeCardsFromHand(c.hand2, p2Cards)
      c.copy(hand1 = p1Hand, hand2 = p2Hand, crib = Crib(p1Cards, p2Cards))
    }
  }

  private def playPegging(c: CribbageContext, player1Turn: Boolean): CribbageContext = {
    val pegCard =
      if (player1Turn)
        c.player1.playPeg(playerCribbageContext(1, c))
      else
        c.player2.playPeg(playerCribbageContext(2, c))
    val hand1 = if (player1Turn) c.hand1.takeCard(pegCard) else c.hand1
    val hand2 = if (!player1Turn) c.hand2.takeCard(pegCard) else c.hand2
    val pile = c.pile.add(pegCard)
    val scoreAdd = scorePegging(pile)
    val score = {
      if (player1Turn) c.score.add((scoreAdd,0))
      else c.score.add((0,scoreAdd))
    }
    val newContext = c.copy(hand1 = hand1, hand2 = hand2, pile = pile, score = score)
    newContext
  }

  def playPegging(c: CribbageContext): Seq[CribbageContext] = {
    playPegging(Seq(c))
  }

  def playPegging(cs: Seq[CribbageContext]): Seq[CribbageContext] = {

    val c = cs.head
    val pileSize = c.pile.getCards.size
    val hand1Size = c.hand1.getCards.size
    val hand2Size = c.hand2.getCards.size
    val seqContext = Seq[CribbageContext]()

    def validContext(c: CribbageContext): Boolean = {
      if (
        (pileSize <= 8) &
          (hand1Size <= 4) &
          (hand2Size <= 4) &
          ( (pileSize + hand1Size + hand2Size) == 8)
      ) true
      else false
    }

    if (pileSize == 8) cs

    else {
      if (!validContext(c)) throw new RuntimeException("Not a valid pegging context" + c.pile.getCards.toString)
      val poneTurn: Boolean =  (pileSize % 2) == 0
      val player1Pone: Boolean = c.dealer != 1
      val player1Turn: Boolean = (poneTurn & player1Pone) || (!poneTurn & !player1Pone)
      val newContext = playPegging(c, player1Turn)
      playPegging(newContext +: cs)
    }


  }

  private def getRankValue(rank: Rank): Int = {
    val rankValues = Map[Rank,Int](
      (Ace, 1),
      (Two, 2),
      (Three, 3),
      (Four, 4),
      (Five, 5),
      (Six, 6),
      (Seven, 7),
      (Eight, 8),
      (Nine, 9),
      (Ten, 10),
      (Jack, 10),
      (Queen, 10),
      (King, 10)
    )
    rankValues(rank)
  }

  private def getNextRank(rank: Rank, direction:Int):  Option[Rank] = {
    val ranks = Array[Rank](Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)
    val nextRank =
      if (direction > 0) {
        if (rank == King) None
        else Some(ranks(ranks.indexOf(rank) + 1))
      }
      else {
        // direction <= 0
        if (rank == Ace) None
        else Some(ranks(ranks.indexOf(rank) - 1))
      }
    nextRank
  }

  private def getSubPile(left: Pile, right: Pile = Pile(), cnt: Int = 0): Pile = {
    if (left.getCards.isEmpty) right
    else {
      val nextCard = left.last()
      val nextCardValue = getRankValue(nextCard.rank)
      val nextLeft = left.init()
      val PossibleNextCnt = cnt + nextCardValue
      val (nextCnt, nextRight) =
        if (PossibleNextCnt > 31) (getRankValue(nextCard.rank), Pile(Seq(nextCard)))
        else (PossibleNextCnt, right.add(nextCard))
      getSubPile(nextLeft, nextRight, nextCnt)
    }

  }

  private def validSubPile(p: Seq[Card]): Boolean = {
    val sumRanks = (for (c <- p) yield getRankValue(c.rank)).sum
    if (sumRanks > 31) {
      false
    }
    else {
      true
    }
  }

  def scorePegging(pile: Pile): Int = {

    def scoreBooks(cards: Seq[Card]): Int = {
      assert(validSubPile(cards), "subpile contains more than 31 points")
      def bookDepth(cards: Seq[Card], topCardRank: Rank, acc: Int): Int =  {
       if (cards.isEmpty) acc
       else {
         val (accNew, nextCards) = cards.head.rank match {
           case `topCardRank` =>(acc + 1, cards.tail)
           case _ => ;(acc, Seq())
         }
         bookDepth(nextCards, topCardRank, accNew)
       }
      }
      val depth = bookDepth(cards.tail,cards.head.rank,1)
      val score = depth match {
        case 2 => 2
        case 3 => 6
        case 4 => 12
        case _ => 0
      }
      score
    }

    def scoreRuns(cards: Seq[Card]): Int = {
      assert(validSubPile(cards), "subpile contains more than 31 points")
      def runDepth(cards: Seq[Card], lastRank: Rank, suite: Suite, direction: Int,  acc: Int): Int = {
        val nextRankOpt = getNextRank(lastRank, direction)
        if (cards.isEmpty || nextRankOpt.isEmpty) acc
        else {
          val nextRank = nextRankOpt.get
          val (accNew, nextCardRank, nextCards) = (cards.head.rank, cards.head.suite) match {
            case (`nextRank`, `suite`) => (acc + 1, nextRank, cards.tail)
            case _ => (acc, None, Seq())
          }
          runDepth(nextCards, nextRank, suite, direction, accNew)
        }
      }
      val depthUp = runDepth(cards.tail, cards.head.rank,cards.head.suite, 1, 1)
      val depth = if (depthUp < 2) runDepth(cards.tail,cards.head.rank,cards.head.suite, -1, 1) else depthUp
      val score = if (depth > 2) depth else 0
      score
    }
    def score15s(cards: Seq[Card]): Int = {
      assert(validSubPile(cards), "subpile contains more than 31 points")
      def is15(cards: Seq[Card], acc: Int): Boolean = {
        if (acc == 15) true
        else if (acc > 15) false
        else if (cards.isEmpty) false
        else {
          is15(cards.init, acc + getRankValue(cards.last.rank))
        }
      }
      if (is15(cards, 0)) 2 else 0
    }

    def score31s(cards: Seq[Card]): Int = {
      val sumRanks = (for (c <- cards) yield getRankValue(c.rank)).sum
      if (sumRanks == 31) 2 else 0
    }

    val cards = getSubPile(pile).getCards

    scoreBooks(cards) + scoreRuns(cards) + score15s(cards) + score31s(cards)

  }
}
