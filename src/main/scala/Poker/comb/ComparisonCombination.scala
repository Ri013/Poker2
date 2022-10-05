package Poker.comb
import Poker.cards.Card
import Poker.play.Player



import cats.instances.vector._
import cats.syntax.semigroup._
case class ComparisonCombination(){
}

  object ComparisonCombination {

    def priortyHighCard(f: Vector[Card] => Vector[Card])(player: Player): Int = {
      val comb: Vector[Card] = f(player.comb.cardsPlayer)
      comb(comb.length-1).dignity.priorityDignity
    }

    def compaireVector (players: Vector[Player]): Vector[Player] = {
      players.foldLeft(Vector.empty[Player])((a, b) =>
        if(a.isEmpty) a.appended(b)
        else if (a.length == 1) compaire(b, a(0))
        else if (a.length == 2)
        {if (compaire(b, a(1)).length == 1) {
          val a1: Vector[Player] = compaire(b, a(1))
          a.distinct |+| a1
        }
        else {
          (a |+| compaire(b, a(1)).drop(1))
        }
        } else if (a.length == 3) {
          if (compaire(b, a(1)).length == 1) {
            val a1 = compaire(b, a(1))
            a.distinct |+| a1
          }
          else {
            a.appended(compaire(b, a(2)).drop(2))
          compaire(b, a(1))
          }
        }
        else compaire(b, a(2)))
    }

    def compaire(player1: Player, player2: Player): Vector[Player] = {
      (player1.comb.priorityCombination, player2.comb.priorityCombination) match {
        case (a, b) if (a > b) => Vector(player1)
        case (a, b) if (a < b) => Vector(player2)
        case (a, b) if (a == b) => moreLess(a)(player1, player2)
      }
    }

    def moreLess(numberComb: Int)(player1: Player, player2: Player): Vector[Player] = {
      numberComb match {
        case 23 => Vector(player1, player2)
        case 22 => highCard(player1, player2)(GetCombination.getStraightFlush)
        case 21 => highCard(player1, player2)(GetCombination.getCare)
        case 20 => {
          if (highCard(player1, player2)(GetCombination.getTroika).length == 1)
            highCard(player1, player2)(GetCombination.getTroika)
          else comparisonPair(player1, player2)(GetCombination.getPair)
        }
        case 19 => highCard(player1, player2)(GetCombination.getFlush)
        case 18 => highCard(player1, player2)(GetCombination.getStraight)
        case 17 => highCard(player1, player2)(GetCombination.getTroika)
        case 16 => comparisonTwoPair(player1, player2)(GetCombination.getTwoPair)
        case 15 => comparisonPair(player1, player2)(GetCombination.getPair)
        case a if (a <= 14) => comparisonHand(player1, player2)
      }
    }

    def comparisonHand(player1: Player, player2: Player): Vector[Player] = {
      if (player1.hand.cardsHand(1).dignity > player2.hand.cardsHand(1).dignity)
        Vector(player1)
      else if (player1.hand < player2.hand)
        Vector(player2)
      else if (player1.hand > player2.hand)
        Vector(player1)
      else if (player1.hand < player2.hand)
        Vector(player2)
      else Vector(player1, player2)
    }

    def comparisonPair(pl1: Player, pl2: Player)(f: Vector[Card] => Vector[Card]): Vector[Player] = {
      if (priortyHighCard(f)(pl1) > priortyHighCard(f)(pl1))
        Vector(pl1)
      else if (priortyHighCard(f)(pl1) < priortyHighCard(f)(pl1))
        Vector(pl2)
      else comparisonHand(pl1, pl2)
    }

    def comparisonTwoPair(player1: Player, player2: Player)(f: Vector[Card] => Vector[Card]): Vector[Player] = {
      val combPl1 = f(player1.comb.cardsPlayer)
      val combPl2 = f(player2.comb.cardsPlayer)
      if (combPl1(3).dignity > combPl2(3).dignity)
        Vector(player1)
      else if (combPl1(3).dignity < combPl2(3).dignity)
        Vector(player2)
      else if (combPl1(1).dignity < combPl2(1).dignity)
        Vector(player2)
      else if (combPl1(1).dignity > combPl2(1).dignity)
        Vector(player1)
      else Vector(player1, player2)
    }

    def highCard(pl1: Player, pl2: Player)(f: Vector[Card] => Vector[Card]): Vector[Player] = {
      if (priortyHighCard(f)(pl1) > priortyHighCard(f)(pl1))
        Vector(pl1)
      else if (priortyHighCard(f)(pl1) < priortyHighCard(f)(pl1))
        Vector(pl2)
      else Vector(pl1, pl2)
    }
  }
