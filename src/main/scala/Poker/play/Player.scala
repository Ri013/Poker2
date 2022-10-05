package Poker.play
 import Poker.comb.Combination
 import cats.Monoid
 import cats.instances.vector._
 import cats.syntax.semigroup._

case class Player(val name: String,val hand: Hand, val tadle: Table)  {

  val comb:Combination = Combination(hand.cardsHand |+| tadle.cardsTable)

}

case object Player{

}


