package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = {
    if(r < 0) 0
    else if(c > r) 0
    else if (c < 0) 0
    else if(r == 0) 1
    else pascal(c-1,r-1)+pascal(c,r-1)
  }

  /**
   * Exercise 2
   *
   * The parentheses are balanced if the outermost
   * parens are balanced and the innermost parens are balanced
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceStack(ch: List[Char], stack: String): Boolean = {
      if(ch.isEmpty) stack.isEmpty
      else if (ch.head=='(') balanceStack(ch.tail, ch.head+stack)
      else if (ch.head==')') {
        if(stack.isEmpty) false
        else balanceStack(ch.tail, stack.tail)
      } else balanceStack(ch.tail, stack)
    }

    balanceStack(chars, "")
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def findchange(remaining: Int, change: List[Int]): Int ={
      if(remaining==0) 1
      else if (remaining < 0) 0
      else if (change.isEmpty) 0
      else {
        findchange(remaining-change.head, change) + findchange(remaining, change.tail)
      }
    }

    if(money==0 || coins.isEmpty) 0
    else findchange(money, coins)
  }
}
