package reductions

import scala.annotation._
import org.scalameter._
import common._

import scala.Char

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def increment(chars: Array[Char]) = {
      val length = chars.length
      if (length == 0) 0
      else if (chars.head.equals('(')) 1
      else if (chars.head.equals(')')) -1
      else 0
    }

    def countUnbalanced(unbalanced: Int, chars: Array[Char]): Int = {
      if (unbalanced < 0) -1
      else if(chars.isEmpty) 0
      else {
        val tail = chars.tail
        if (tail.isEmpty) unbalanced
        else countUnbalanced(unbalanced + increment(tail), tail)
      }
    }

    countUnbalanced(increment(chars), chars) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def trueReduction(a: (Int, Int), b: (Int, Int)): (Int, Int) = {
      val unbalancedOpening = a._1 + b._1 - b._2
      val unbalancedClosing = a._2 + math.max(0, b._2 - a._1)
      (unbalancedOpening, unbalancedClosing)
    }

    /**
      * counts unbalanced parenthesis in a string
      * result is represented as a pair(unbalancedOpening, unbalancedClosing)
      * one of the values in the pair is 0
      */
    def traverse(idx: Int, until: Int): (Int, Int) = {

      if (until <= idx) (0, 0)
      else {
        var opening = 0
        var closing = 0
        for (index <- idx until until) {
          chars(index) match {
            case '(' =>
              opening += 1
            case ')' =>
              if(opening == 0) closing += 1
              else opening -=1
            case _ =>
          }
        }

        (opening, closing)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until <= from) (0, 0)
      else {
        val bounds: Seq[Int] = (from until until).by(threshold) :+ until
        val tasks = for (i <- bounds.indices.dropRight(1))
          yield task(traverse(bounds(i), bounds(i + 1)))
        tasks.map(_.join()).reduce(trueReduction)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }
}
