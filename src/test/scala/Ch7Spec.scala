package fpclub7

import Ch7._
import org.scalatest.{FlatSpec, Matchers}

class Ch7Spec extends FlatSpec with Matchers {

  val ex = new ExecutorService

  "parFilter" should "just work" in {
    val list = List(1,2,3,4,5,6,7,8)

    Par.parFilter(list)(_%2 == 0)(ex).get should be(List(2,4,6,8))
  }

  "parCountWords" should "just work" in {
    val corpus = List(
      "hello world, how are you?",
      "i am fine.\n\nhere is another line.",
      "thanks, i already ate though."
    )

    Par.parCountWords(corpus)(ex).get should be(17)
  }

}
