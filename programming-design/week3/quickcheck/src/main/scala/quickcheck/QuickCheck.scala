package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec
import scala.util.Random

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    val heap = empty

    val heaps = for {
      someA <- -100 until 100
    } yield insert(someA, heap)

    Gen.oneOf(heaps)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  @tailrec
  private def deleteAll(heap: H): H = {
    if(isEmpty(heap)) heap
    else deleteAll(deleteMin(heap))
  }

  property("findMin returns the only value in queue") = forAll {
    (h: H) => {
      val emptyHeap: H = deleteAll(h)
      val value = 1
      value == findMin(insert(value, emptyHeap))
    }
  }

  property("findMin returns the min value in the queue") = forAll {
    (h: H) => {
      findMin(insert(Int.MinValue, h)) == Int.MinValue
    }
  }

  property("deleteMin deletes min value from the queue") = forAll {
    (h: H) => {
      val prevMin = findMin(h)
      findMin(deleteMin(insert(Int.MinValue, h))) == prevMin
    }
  }

  property("deleteMin deletes only one item from the queue") = forAll {
    (h: H) => {
      findMin(deleteMin(insert(Int.MinValue + 2, insert(Int.MinValue + 1, h)))) == Int.MinValue + 2
    }
  }

  property("deleteMin deletes only one item from the queue") = forAll {
    (h: H) => {
      findMin(deleteMin(insert(Int.MinValue,insert(Int.MinValue, h)))) == Int.MinValue
    }
  }

  property("isEmpty should return false on non empty heap") = forAll {
    (h: H) => {
      !isEmpty(insert(Int.MaxValue, h))
    }
  }

  property("isEmpty should return true if heap is empty") = forAll {
    (h: H) => {

      @tailrec
      def deleteAll(heap: H): H = {
        if(isEmpty(heap)) heap
        else deleteAll(deleteMin(heap))
      }

      val potentiallyEmpty = deleteAll(h)
      val withSingleElement = insert(Int.MaxValue, potentiallyEmpty)
      findMin(withSingleElement)==Int.MaxValue && isEmpty(deleteMin(withSingleElement))
    }
  }

  property("findMin should return lesser of min elements before meld") = forAll {
    (h: H) => {
      val otherQueue = insert(Int.MinValue, h)
      findMin(meld(h, otherQueue)) == Int.MinValue
    }
  }

  property("meld result should contain same count of elements as source queues together") = forAll {
    (h: H) => {
      val empty = deleteAll(h)
      val other = insert(Int.MinValue, empty)
      val nonEmpty = insert(Int.MinValue, empty)

      isEmpty(deleteMin(deleteMin(meld(nonEmpty, other))))
    }
  }

  property("insert should insert only one element") = forAll {
    (h: H) => {
      val empty = deleteAll(h)
      isEmpty(deleteMin(insert(0, empty)))
    }
  }

}
