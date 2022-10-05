package Poker.play

import Poker.cards.{Card, Dignity, Suits}

final case class Hand( cardsHand: Vector[Card]) {

  def comparison (d2: Hand): Int = {
    if (this.cardsHand(cardsHand.length).dignity > d2.cardsHand(cardsHand.length).dignity)
      1
    else if (this.cardsHand(cardsHand.length).dignity < d2.cardsHand(cardsHand.length).dignity)
      - 1
    else 0
  }

  def > (h2: Hand): Boolean = {
    comparison (h2) == 1
  }
  def < (h2: Hand): Boolean = {
    comparison (h2) == - 1
  }
}

