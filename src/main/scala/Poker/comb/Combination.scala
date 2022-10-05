package Poker.comb

import Poker.cards.{Card, Dignity, Suits}
import Poker.comb.Combination.{combination, getHighCard, thisCare, thisFlush, thisFullHouse, thisPair, thisStraight, thisTwoPair, thisRoyalFlush, thisStraightFlush, thisTroika}
import cats.data.Reader

import scala.annotation.tailrec

case class Combination(cardsPlayer: Vector[Card]){

  val nameCombination: String = bestCombination()

  val priorityCombination: Int = combination(nameCombination)

  def bestCombination(): String = {
    if (thisRoyalFlush(cardsPlayer))
      "RoyalFlush"
      else if (thisStraightFlush(cardsPlayer))
        "StraightFlush"
      else if (thisCare(cardsPlayer))
        "Care"
      else if (thisFullHouse(cardsPlayer))
        "FullHouse"
      else if (thisFlush(cardsPlayer))
        "Flush"
      else if (thisStraight(cardsPlayer))
        "Straight"
      else if (thisTroika(cardsPlayer))
        "Troika"
      else if (thisTwoPair(cardsPlayer))
        "TwoPair"
      else if (thisPair(cardsPlayer))
        "Pair"
      else
        getHighCard(cardsPlayer)
  }
}

object Combination {

  val combination = Map(
    "Two" -> 2,
    "Three" -> 3,
    "Four" -> 4,
    "Five" -> 5,
    "Six" -> 6,
    "Seven" -> 7,
    "Eight" -> 8,
    "Nine" -> 9,
    "Ten" -> 10,
    "Jack" -> 11,
    "Queen" -> 12,
    "King" -> 13,
    "Ace" -> 14,
    "Pair" -> 15,
    "Two Pair" -> 16,
    "Troika" -> 17,
    "Straight" -> 18,
    "Flush" -> 19,
    "FullHouse" -> 20,
    "Care" -> 21,
    "StraightFlush" -> 22,
    "RoyalFlush" -> 23)

  @tailrec
  def checkCombination(n: Int, counter: Int)(list: Vector[Card])
                      (f1: (Int, Int, Vector[Card]) => Boolean)(f2: (Int, Int, Vector[Card]) => Boolean): Boolean = {
    if (n < list.length) {
      list(n) match {
        case c if (f1(counter, n, list)) => true
        case c if (f2(counter, n, list)) => checkCombination(n + 1, counter + 1)(list)(f1)(f2)
        case c  => checkCombination(n + 1,0)(list)(f1)(f2)
      }
    }
    else false
  }

  def thisRoyalFlush(card: Vector[Card]): Boolean = {
    checkCombination(0, 0)(Card.sortCardOnSuit(card))((counter, n, card)
    => (counter == 4 && card(n - 1).suit == card(n).suit && card(n).dignity.==(card(n - 1).dignity + 1)))((counter, n, card)
    => (n != card.length - 1 && card(n + 1).suit == card(n).suit
        && card(n + 1).dignity == card(n).dignity + 1 && card(n).dignity == (10 + counter)))
  }

  def thisStraightFlush(card: Vector[Card]): Boolean = {
    checkCombination(0, 0)(Card.sortCardOnSuit(card))((counter, n, card)
    => (counter == 4 && n <= card.length - 1 && card(n).suit == card(n - 1).suit && card(n).dignity == card(n - 1).dignity + 1))((counter, n, card)
    => (n != card.length - 1 && card(n + 1).suit == card(n).suit && card(n + 1).dignity == card(n).dignity))
  }

  def thisCare(card: Vector[Card]): Boolean = {
    checkCombination(0, 0)(Card.sortDignity(card))((counter, n, card)
    => (counter == 3 && n <= card.length - 1 && card(n).dignity == card(n - 1).dignity))((counter, n, card)
    => (n != card.length - 1 && card(n + 1).dignity == card(n).dignity))
  }

  def thisFullHouse(cards: Vector[Card]): Boolean = {
    if (thisTroika(cards) && thisPair(cards))
      true
    else false
  }

  def thisFlush(card: Vector[Card]): Boolean = {
    checkCombination(0, 0)(Card.sortDignity(card))((counter, n, card)
    => (counter == 4 && n <= card.length - 1 && card(n).suit == card (n - 1).suit))((counter, n, card)
    => (n != card.length - 1 && card (n + 1).suit == card(n).suit))
  }

  def thisStraight(cards: Vector[Card]): Boolean = {
    @tailrec
    def examination(n: Int)(counter: Int)(v: Vector[Card]): Boolean = {
      if (n < v.length) {
        v(n) match {
          case c if (counter == 4 && n <= v.length - 1 && c.dignity == v(n - 1).dignity + 1) => true
          case c if (n != v.length - 1 && v(n + 1).dignity == c.dignity + 1) => examination(n + 1)(counter + 1)(v)
          case c if (n != v.length - 1 && v(n + 1).dignity == c.dignity) => examination(n + 1)(counter)(v)
          case c => examination(n + 1)(0)(v)
        }
      }
      else false
    }
    examination(0)(0)(Card.sortDignity(cards))
  }

  def thisTroika(card: Vector[Card]): Boolean = {
    checkCombination(0, 0)(Card.sortDignity(card))((counter, n, card)
    => (n <= card.length - 3 && card(n + 1).dignity == card(n).dignity && card(n + 2).dignity == card(n).dignity))((counter, n, card)
    => true)
  }

  def thisTwoPair(cards: Vector[Card]): Boolean = {
    @tailrec
    def examination(n: Int)(counter: Int)(v: Vector[Card]): Boolean = {

      if (n < v.length) {
        v(n) match {
          case c if (n != 0 && v(n - 1).dignity == c.dignity && counter == 1) => true
          case c if (n != 0 && v(n - 1).dignity == c.dignity) => examination(n - 2)(counter + 1)(v)
          case c if (n == 0) => false
          case c => examination(n - 1)(counter)(v)
        }
      }
      else false
    }

    examination(cards.length - 1)(0)(Card.sortDignity(cards))
  }

   def thisPair(cards: Vector[Card]): Boolean = {
    @tailrec
    def examination(n: Int)(v: Vector[Card]): Boolean = {
      if (n < v.length) {
        v(n) match {
          case c if (n <= v.length - 3 && v(n + 2).dignity == c.dignity && v(n + 1).dignity == c.dignity) => examination(n + 2)(v)
          case c if (n < v.length - 1 && v(n + 1).dignity == c.dignity) => true
          case c if (n == v.length - 1) => false
          case c => examination(n + 1)(v)
        }
      }
      else false
    }
    examination(0)(Card.sortDignity(cards))
  }

  def getHighCard(cards: Vector[Card]): String = {
    Card.sortDignity(cards)(cards.length-1).dignity.nameDignity
  }
}
