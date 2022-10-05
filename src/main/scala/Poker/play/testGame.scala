package Poker.play

import Poker.cards.{Card, Dignity, Suits}
import Poker.comb.ComparisonCombination

import scala.util.Random

object testGame extends App{
  val hand1 = Hand(Vector(Card(Suits("Diamond"), Dignity ("Three")), Card(Suits("Diamond"), Dignity ("Four"))))

  val hand2 = Hand(Vector(Card(Suits("Club"), Dignity ("Six")), Card(Suits("Spade"), Dignity ("Nine"))))

  val table = Table(Vector(Card(Suits("Diamond"), Dignity ("Jack")), Card(Suits("Diamond"), Dignity ("Ace")),
    Card(Suits("Spade"), Dignity ("Ace")),Card(Suits("Diamond"), Dignity ("Seven")),Card(Suits("Heart"), Dignity ("Ten"))))

  val player1 = Player("Rim", hand1, table)
  val player2 = Player("Tom", hand2, table)
  println(player1.comb)
   println(player2.comb)
  println(player1.comb.nameCombination)
  println(player2.comb.nameCombination)
println(ComparisonCombination.compaireVector(Vector(player1, player2)))
}

//val prioritySuits = Map (
//"Diamond" -> 1,
//"Heart" -> 2,
//"Club" -> 3,
//"Spade" -> 4
//)Vector("Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King", "Ace")