package Poker.cards

import Poker.cards.Card.prioritySuits

case class Card(suit: Suits, dignity: Dignity) {
}

object Card {

  def sortDignity(vector: Vector[Card]): Vector[Card] = {
    vector.sortWith((x, y) => x.dignity < y.dignity)
  }

  def sortCardOnSuit(l: Vector[Card]): Vector[Card] = {
    (l.sortWith((x, y) => x.suit < y.suit)).sortWith((x, y) => if (x.suit == y.suit) x.dignity < y.dignity else false)
  }
//  implicit val ord: Ordering[Card] = new Ordering[Card] {
//    override def compare(x: Card, y: Card): Int = {
//      (x, y) match {
//        case (x,y) if (x.dignity > y.dignity && x.suit > y.suit) => 1
//        case (x,y) if (x.dignity > y.dignity && x.suit == y.suit) => 1
//        case (x,y) if (x.dignity > y.dignity && x.suit < y.suit) => -1
//        case (x,y) if (x.dignity < y.dignity) => -1
//        case (x,y) if (x.dignity < y.dignity) => -1
//        case _ => 0
//      }
//    }
//  }

  val prioritySuits = Map (
    "Diamond" -> 1,
    "Heart" -> 2,
    "Club" -> 3,
    "Spade" -> 4
  )

}
