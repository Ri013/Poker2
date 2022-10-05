package Poker.cards

import Poker.comb
import Poker.comb.Combination

case class Dignity (nameDignity: String){

 val priorityDignity = Combination.combination(nameDignity)

def comparison (d2: Dignity): Int = {
 if (this.priorityDignity > d2.priorityDignity)
 1
 else if (this.priorityDignity < d2.priorityDignity)
 - 1
 else 0
 }

 def > (d2: Dignity): Boolean = {
  comparison (d2) == 1
 }
 def < (d2: Dignity): Boolean = {
  comparison (d2) == - 1
 }
 def == (i: Int): Boolean = {
  this.priorityDignity == i
}
 def  == (d2: Dignity): Boolean = {
  comparison(d2) == 0
 }

 def + (i: Int): Int ={
  this.priorityDignity + i
 }
}








