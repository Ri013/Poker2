package Poker.comb
import Poker.cards.Card
import Poker.play.Player
import cats.instances.vector._
import cats.syntax.semigroup._

import scala.annotation.tailrec
import scala.concurrent.Future

case class GetCombination() {
}

object GetCombination{

   val getStraightFlush: Vector[Card] => Vector[Card] = (cards: Vector[Card]) => {

    @tailrec
    def examination(n: Int)(counter: Int)(v: Vector[Card]): Vector[Card] = {
      if (n < v.length) {
        v(n) match {
          case c if (counter == 4 && n <= v.length - 1 && c.suit == v(n - 1).suit && c.dignity == v(n - 1).dignity + 1) => v.slice(n - 4, n + 1)
          case c if (n != v.length - 1 && v(n + 1).suit == c.suit && v(n + 1).dignity == c.dignity + 1) => examination(n + 1)(counter + 1)(v)
          case c => examination(n + 1)(0)(v)
        }
      }
      else Vector.empty
    }
    examination(0)(0)(Card.sortCardOnSuit(cards))
  }

    val getCare: Vector[Card] => Vector[Card] = (cards: Vector[Card]) => {
    @tailrec
    def examination(n: Int)(counter: Int)(v: Vector[Card]): Vector[Card] = {
      if (n < v.length) {
        v(n) match {
          case c if (counter == 3 && n <= v.length - 1 && c.dignity == v(n - 1).dignity) => v.slice(n - 3, n + 1)
          case c if (n != v.length - 1 && v(n + 1).dignity == c.dignity) => examination(n + 1)(counter + 1)(v)
          case c => examination(n + 1)(0)(v)
        }
      }
      else Vector.empty
    }
    examination(0)(0)(Card.sortDignity(cards))
  }

  val getFullHouse: Vector[Card] => Vector[Card] = (cards: Vector[Card]) =>{
    getTroika(cards) |+| getPair(cards)
  }

  val getFlush: Vector[Card] => Vector[Card] = (cards: Vector[Card]) => {
    @tailrec
    def examination(n: Int)(counter: Int)(v: Vector[Card]): Vector[Card] = {
      if (n < v.length) {
        v(n) match {
          case c if (counter == 4 && n <= v.length - 1 && c.suit == v(n - 1).suit) => v.slice(n - 4, n + 1)
          case c if (n != v.length - 1 && v(n + 1).suit == c.suit) => examination(n + 1)(counter + 1)(v)
          case c => examination(n + 1)(0)(v)
        }
      }
      else Vector.empty
    }

    examination(0)(0)(Card.sortCardOnSuit(cards))
  }

  val getStraight: Vector[Card] => Vector[Card] = (cards: Vector[Card]) => {
    @tailrec
    def examination(n: Int)(counter: Int)(v: Vector[Card]): Vector[Card] = {
      if (n < v.length) {
        v(n) match {
          case c if (counter == 4 && n <= v.length - 1 && c.dignity == v(n - 1).dignity + 1) => v.slice(n - 4, n + 1)
          case c if (n != v.length - 1 && v(n + 1).dignity == c.dignity + 1) => examination(n + 1)(counter + 1)(v)
          case c if (n != v.length - 1 && v(n + 1).dignity == c.dignity) =>
            examination(n)(counter)(v.slice(0, n) |+| v.slice(n + 1, v.length))
          case c => examination(n + 1)(0)(v)
        }
      }
      else Vector.empty
    }

    examination(0)(0)(Card.sortDignity(cards))
  }

  val getTroika: Vector[Card] => Vector[Card] = (cards: Vector[Card]) =>  {
    @tailrec
    def examination(n: Int)(v: Vector[Card]): Vector[Card] = {
      if (n < v.length) {
        v(n) match {
          case c if (n <= v.length - 3 && v(n + 1).dignity == c.dignity && v(n + 2).dignity == c.dignity) => v.slice(n, n + 3)
          case c => examination(n + 1)(v)
        }
      }
      else Vector.empty
    }

    examination(0)(Card.sortDignity(cards))
  }

  val getTwoPair: Vector[Card] => Vector[Card] = (cards: Vector[Card]) => {
    @tailrec
    def examination(n: Int, counter: Int)(v: Vector[Card])(vect: Vector[Card]): Vector[Card] = {
      if (n < vect.length) {

        vect(n) match {
          case c if (n != 0 && vect(n - 1).dignity == c.dignity && counter == 1) => v.appendedAll(vect.slice(n - 1, n + 1)).reverse
          case c if (n != 0 && vect(n - 1).dignity == c.dignity) => examination(n - 2, counter + 1)(vect.slice(n - 1, n + 1))(vect)
          case c if (n == 0) => Vector.empty
          case c => examination(n - 1, counter)(v)(vect)
        }
      }
      else Vector.empty
    }

    examination(cards.length - 1, 0)(Vector.empty)(Card.sortDignity(cards))
  }

  val getPair: Vector[Card] => Vector[Card] = (cards: Vector[Card]) => {
    @tailrec
    def examination(n: Int)(v: Vector[Card]): Vector[Card] = {
      if (n < v.length) {
        v(n) match {
          case c if (n <= v.length - 3 && v(n + 2).dignity == c.dignity && v(n + 1).dignity == c.dignity) => examination(n + 2)(v)
          case c if (n < v.length - 1 && v(n + 1).dignity == c.dignity) => v.slice(n, n + 2)
          case c if (n == v.length - 1) => Vector.empty
          case c => examination(n + 1)(v)
        }
      }
      else Vector.empty
    }

    examination(0)(Card.sortDignity(cards))
  }

  def getHighCard(cards: Vector[Card]): String = {
    Card.sortDignity(cards)(cards.length - 1).dignity.nameDignity
  }
}
