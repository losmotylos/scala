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
    if (c == 0 && r == 0) 1
    else if (c < 0 || r < 0) 0
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(acc: Int, charList: List[Char]): Int = {
      if (charList.isEmpty) acc
      else {
        if(charList.head.equals(')') && acc -1 < 0 ) -1
        else if (charList.head.equals('(')) loop(acc + 1, charList.tail)
        else if (charList.head.equals(')')) loop(acc - 1, charList.tail)
        else loop(acc, charList.tail)
      }
    }
    if(loop(0, chars) == 0) true else false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    println("money: " + money + " coins: " + coins)
    if(money == 0) 1
    else if(money < 0) 0
    else if(coins.size == 0) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
