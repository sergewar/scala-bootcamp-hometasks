package com.sss.scalabootcamp.hometasks.task5

object ADT {
  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suite
  sealed trait Suite
  object Suite {
    final case object Clubs extends Suite
    final case object Diamonds extends Suite
    final case object Hearts extends Suite
    final case object Spades extends Suite
  }

  // 2. Rank
  final case class Rank private (value: Int) extends AnyVal
  object Rank {
    def create(value: Int): Option[Rank] =
      value match {
        case v if v >= 2 && v <= 14 => Some(Rank(v))
        case _                      => None
      }
  }

  // 3. Card
  final case class Card(suite: Suite, rank: Rank)

  // 4. Hand (Texas or Omaha)
  final case class Hand private (handCards: List[Card]) extends AnyVal
  object Hand {
    def create(handCards: List[Card], isOmaha: Boolean): Option[Hand] =
      handCards match {
        case l if l.length == 2 && !isOmaha => Some(Hand(l))
        case l if l.length == 4 && isOmaha  => Some(Hand(l))
        case _                              => None
      }
  }

  // 5. Board
  final case class Board private (boardCards: List[Card]) extends AnyVal
  object Board {
    def create(boardCards: List[Card]): Option[Board] =
      boardCards match {
        case l if l.length == 5 => Some(Board(l))
        case _                  => None
      }
  }

  // 6. Poker Combination (High Card, Pair, etc.)
  sealed trait PokerCombination
  object PokerCombination {
    final case object Highcard extends PokerCombination
    final case object Pair extends PokerCombination
    final case object TwoPairs extends PokerCombination
    final case object ThreeOfAKind extends PokerCombination
    final case object Straight extends PokerCombination
    final case object Flush extends PokerCombination
    final case object FullHouse extends PokerCombination
    final case object FourOfAKind extends PokerCombination
    final case object StraightFlush extends PokerCombination
  }

  // 7. Test Case (Board & Hands to rank)
  final case class Case(board: Board, hands: List[Hand])
  object Case {
    def create(
        board: Board,
        hands: List[Hand],
        isOmaha: Boolean
    ): Option[Case] =
      hands match {
        case h if !isOmaha && h.nonEmpty && h.length <= 22 =>
          Some(Case(board, hands))
        case h if isOmaha && h.nonEmpty && h.length <= 10 =>
          Some(Case(board, hands))
        case _ => None
      }
  }

  // 8. Test Result (Hands ranked in a particular order)
  final case class HandWithRank(
      rank: Int,
      hands: Hand,
      pokerCombination: PokerCombination,
      combinationCards: List[Card]
  )
  object HandWithRank {
    def create(
        rank: Int,
        hands: Hand,
        pokerCombination: PokerCombination,
        combinationCards: List[Card]
    ): Option[HandWithRank] =
      if (combinationCards.length == 5)
        Some(HandWithRank(rank, hands, pokerCombination, combinationCards))
      else None
  }

  final case class Result(handsWithRank: List[HandWithRank])
  object Result {
    def create(handsWithRank: List[HandWithRank]): List[HandWithRank] =
      handsWithRank.sortBy(handsWithRank => handsWithRank.rank)
  }

}
