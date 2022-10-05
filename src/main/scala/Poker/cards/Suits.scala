package Poker.cards

import Poker.cards.Suits.prioritySuits

case class Suits (name: String) {

  val numberSuits = prioritySuits(name)

    def comparison(s2: Suits): Int = {
      if (this.numberSuits > s2.numberSuits)
        1
      else if (this.numberSuits < s2.numberSuits)
        -1
      else 0
    }

    def > (s2: Suits): Boolean = {
      comparison(s2) == 1
    }

    def < (s2: Suits): Boolean = {
      comparison(s2) == -1
    }

    def == (s2: Suits): Boolean = {
      comparison(s2) == 0
  }
}

object Suits {
  val prioritySuits = Map(
    "Diamond" -> 1,
    "Heart" -> 2,
    "Club" -> 3,
    "Spade" -> 4
  )
}



