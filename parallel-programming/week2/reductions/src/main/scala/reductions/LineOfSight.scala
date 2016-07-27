package reductions

import org.scalameter._
import common._

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def angle(nodeNumber: Int, height: Float) = height / nodeNumber
  /**
    * For each node except the first one (observer position) calculates min angle tangents where it still can be seen
    *
    * @param input - heights of nodes
    * @param output - corresponding tan(angle)
    */
  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {

    var maxTillNow = 0.0F

    for (pointNumber <- input.indices.drop(1)) {
      val height = input(pointNumber)
      maxTillNow = math.max(maxTillNow, angle(pointNumber, height))
      output(pointNumber) = maxTillNow
    }
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /**
    * Traverses the specified part of the array and returns the maximum angle.
    */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    var max = 0.0F
    for(index <- from until until) {
      val nodeAngle = angle(index, input(index))
      if (nodeAngle > max) max = nodeAngle
    }
    max
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int, threshold: Int): Tree = {
    if(end - from < threshold)
      new Leaf(from, end, upsweepSequential(input, from, end))
    else {
      //create a node
      val mid = (end - from) / 2 + from
      val left = upsweep(input, from, mid, threshold)
      val right = upsweep(input, mid, end, threshold)

      new Node(left, right)
    }
  }

  /*
    * The second phase is called downsweep -- here, the algorithm uses the tree to push the maximum angle in
    * the corresponding prefix of the array to the leaves of the tree, and outputs the values.
    * Implement the methods downsweep which processes parts of the tree in parallel, and the method downsweepSequential,
    * which traverses the parts of the array corresponding to leaves of the tree and writes the final
    * angles into the output array:
    */

  /** Traverses the part of the `input` array starting at `from` and until
    * `until`, and computes the maximum angle for each entry of the output array,
    * given the `startingAngle`.
    *
    * @param input         - array of heights
    * @param output        - (array of max node angle up to (including) node in concern)
    * @param startingAngle - max angle of previous nodes
    * @param from          - start index
    * @param until         - end index
    */
  def downsweepSequential(input: Array[Float], output: Array[Float], startingAngle: Float, from: Int, until: Int): Unit = {
    for(index <- from until until) {
      val res = upsweepSequential(input, from, index + 1)
//      println(s"from=$from index=$index res=$res")
      output(index) = max(startingAngle, res)
    }
  }

  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepTraverse` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float, tree: Tree): Unit = {

    tree match {
      case Leaf(from, until, maxPrevious) =>
        downsweepSequential(input, output, maxPrevious, from, until)
      case Node(left, right) =>
        parallel(downsweep(input, output, left.maxPrevious, left), downsweep(input, output, right.maxPrevious, right))
    }
  }

  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    val reductionTree: Tree = upsweep(input, 0, input.length, threshold)
    downsweep(input, output, 0F, reductionTree)
  }
}
