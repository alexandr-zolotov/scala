package recfun

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
    if (c > r) 0
    else if (c == 0) 1
    else {
      val prevRow = r - 1
      pascal(c - 1, prevRow) + pascal(c, prevRow)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def increment(chars: List[Char]) = {
      val length = chars.length
      if (length == 0) 0
      else if (chars.head.equals('(')) 1
      else if (chars.head.equals(')')) -1
      else 0
    }

    def countUnbalanced(unbalanced: Int, chars: List[Char]): Int = {
      if (unbalanced < 0) -1
      else {
        val tail = chars.tail
        if (tail.isEmpty) unbalanced
        else countUnbalanced(unbalanced + increment(tail), tail)
      }
    }

    countUnbalanced(increment(chars), chars) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.sorted.reverse

    //result is a sum of
    // a) combinations of current n coins + remainder of smaller coins and
    // b) combinations for current money of smaller coins


    def count(money: Int, sortedCoins: List[Int]): Int = {
      //no more coins - no combinations
      if (sortedCoins.isEmpty) 0
      else {
        //get biggest coin
        val coin: Int = sortedCoins.head
        val currentMoneySmallerCoins: Int = count(money, sortedCoins.tail)
        //if coin is bigger than amount of money - proceed with smaller coins
        if (money < coin) currentMoneySmallerCoins
        //if coin is equal to the amount of money - add this single combination adn proceed with smaller coins
        else if (money == coin) currentMoneySmallerCoins + 1
        else {
          val currentMoneyReducedCurrentCoin = count(money - coin, sortedCoins)
          if (sortedCoins.tail.isEmpty) currentMoneyReducedCurrentCoin
          else currentMoneyReducedCurrentCoin + currentMoneySmallerCoins
        }
      }
    }

    count(money, sortedCoins)
  }

}
