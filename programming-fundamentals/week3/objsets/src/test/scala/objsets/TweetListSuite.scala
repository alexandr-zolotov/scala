package objsets

import org.scalatest.FunSuite

/**
  * @author Alexandr Zolotov
  */
class TweetListSuite extends FunSuite{

  test("concat non empty"){
    val tweet1: Tweet = new Tweet("u", "a", 1)
    val tweet2: Tweet = new Tweet("u", "b", 1)
    val tweet3: Tweet = new Tweet("u", "c", 1)
    val tweet4: Tweet = new Tweet("u", "c", 1)
    val tweet5: Tweet = new Tweet("u", "d", 1)


    val first = new Cons(tweet1, new Cons(tweet2, new Cons(tweet3, Nil)))
    val second = new Cons(tweet4, new Cons(tweet5, Nil))

    val result: TweetList = first.concat(second)

    assert(result.head.text == "a")
    assert(result.end.text == "d")
  }

}
