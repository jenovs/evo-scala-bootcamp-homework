package adt

object Homework extends App {
  // Homework 5. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  sealed trait Suit
  object Suit {
    case object Clubs extends Suit
    case object Diamonds extends Suit
    case object Hearts extends Suit
    case object Spades extends Suit

    private val suits =
      Map("c" -> Clubs, "d" -> Diamonds, "h" -> Hearts, "s" -> Spades)

    def of(s: String): Either[String, Suit] = {
      if (!suits.contains(s.toLowerCase())) Left("Invalid suit")
      else Right(suits(s.toLowerCase))
    }
  }

  sealed trait Rank
  object Rank {
    case object Two extends Rank
    case object Three extends Rank
    case object Four extends Rank
    case object Five extends Rank
    case object Six extends Rank
    case object Seven extends Rank
    case object Eight extends Rank
    case object Nine extends Rank
    case object Ten extends Rank
    case object Jack extends Rank
    case object Queen extends Rank
    case object King extends Rank
    case object Ace extends Rank

    private val ranks = Map(
      "2" -> Two,
      "3" -> Three,
      "4" -> Four,
      "5" -> Five,
      "6" -> Six,
      "7" -> Seven,
      "8" -> Eight,
      "9" -> Nine,
      "T" -> Ten,
      "J" -> Jack,
      "Q" -> Queen,
      "K" -> King,
      "A" -> Ace
    )

    def of(r: String): Either[String, Rank] = {
      if (!ranks.contains(r.toUpperCase)) Left("Invalid rank")
      else Right(ranks(r.toUpperCase))
    }
  }

  case class Card(r: Rank, s: Suit)
  case object Card {
    def create(rs: String): Either[String, Card] = {
      val r = Rank.of(rs(0).toString)
      val s = Suit.of(rs(1).toString)

      r match {
        case Left(err) => Left(err)
        case Right(rank) =>
          s match {
            case Left(err)   => Left(err)
            case Right(suit) => Right(Card(rank, suit))
          }
      }
    }
  }

  abstract class Hand(hand: String*)
  case object Hand {
    def apply(input: String) = {
      if (input.length % 2 != 0) Left("Invalid hand length")
      else {
        input
          .grouped(2)
          .toList
          .map(h => {
            Card.create(h)
          })
          .map {
            case Left(value)  => value
            case Right(value) => value
          }
      }
    }
  }

  case class Texas(c1: Card, c2: Card)
  case class Omaha(c1: Card, c2: Card, c3: Card, c4: Card)
  case class Board(c1: Card, c2: Card, c3: Card, c4: Card, c5: Card)

  sealed trait Combination {
    def strength: Int
  }
  object Combination {
    case object HighCard extends Combination {
      override val strength = 1
    }
    case object Pair extends Combination {
      override val strength: Int = 2
    }
    case object TwoPairs extends Combination {
      override val strength: Int = 3
    }
    case object ThreeOfKind extends Combination {
      override val strength: Int = 4
    }
    case object Straight extends Combination {
      override val strength: Int = 5
    }
    case object Flush extends Combination {
      override val strength: Int = 6
    }
    case object FullHouse extends Combination {
      override val strength: Int = 7
    }
    case object FourOfKind extends Combination {
      override val strength: Int = 8
    }
    case object StraightFlush extends Combination {
      override val strength: Int = 9
    }
    case object RoyalFlush extends Combination {
      override val strength: Int = 10
    }

    def of(board: Hand, hand: Hand) = ???
  }

  case class TestResult(result: String) extends AnyVal

  case class Test(input: String)
  case object Test {
    def apply(input: String): TestResult = {
      val board :: hands = input.split(" ").toList
      // Hand(board)
      hands.map(hand => Hand(hand))
      TestResult("")
    }
  }
}
