package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, _) => 1
    case (_, _) if c == r => 1
    case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def balance(chars: List[Char], count: Int): Boolean = chars match {
      case Nil => count == 0
      case c :: tail if c == '(' => balance(tail, count + 1)
      case c :: tail if c == ')' => if (count > 0) balance(tail, count - 1) else false
      case _ :: tail => balance(tail, count)
    }

    balance(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    coins.sorted match {
      case Nil => 0
      case coin :: tail if money - coin < 0 => 0
      case coin :: tail if money - coin == 0 => 1
      case coin :: tail if money - coin > 0 => countChange(money - coin, coins) + countChange(money, tail)
    }
  }
}
