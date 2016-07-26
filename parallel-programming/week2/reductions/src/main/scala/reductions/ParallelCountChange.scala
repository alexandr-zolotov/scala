package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 20
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    val sortedCoins = coins.sorted
    count(money, sortedCoins)
  }

  def count(money: Int, sortedCoins: List[Int]): Int = {
    if (money == 0) 1
    else if (sortedCoins.isEmpty || sortedCoins.head > money) 0
    else {
      //get next coin
      val coin: Int = sortedCoins.head
      val currentMoneyBiggerCoins: Int = count(money, sortedCoins.tail)
      val currentMoneyReducedCurrentCoin = count(money - coin, sortedCoins)

      if (sortedCoins.tail.isEmpty) currentMoneyReducedCurrentCoin
      else currentMoneyReducedCurrentCoin + currentMoneyBiggerCoins
    }
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    val sortedCoins = coins.sorted

    def countParallel(leftMoney: Int, sortedCoins: List[Int]): Int = {

      if (threshold(leftMoney, sortedCoins)) count(leftMoney, sortedCoins)
      else if (sortedCoins.isEmpty) 0
      else {
        val coin: Int = sortedCoins.head
        val (currentMoneyBiggerCoins, currentMoneyReducedCurrentCoin) =
          parallel(countParallel(leftMoney, sortedCoins.tail), countParallel(leftMoney - coin, sortedCoins))

        if (sortedCoins.tail.isEmpty) currentMoneyReducedCurrentCoin
        else currentMoneyReducedCurrentCoin + currentMoneyBiggerCoins
      }
    }
    countParallel(money, sortedCoins)
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold =
    (money: Int, coins: List[Int]) => money < ((startingMoney * 2) / 3)

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    ???


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    ???
  }
}
