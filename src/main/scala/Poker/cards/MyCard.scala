package Poker.cards

import Poker.play.Player

case class MyCard () {
}

object MyCard {

//  def desk(): Vector[Card] = {
//    suits.flatMap(a => {
//      significance.foldRight(Vector.empty[Card])((x, y) => y.appended(a, x))
//    })
//  }

 private val significance = Vector("Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King", "Ace")
 private val suits = Vector("Diamond", "Heart", "Club", "Spade")


}


