package fpclub
import org.scalatest.{FlatSpec, Matchers}

class Ch2Spec extends FlatSpec with Matchers {

  "isSorted" should "work for 0-sized arrays" in {
    val as = Array() : Array[Int]
    Ch2.isSorted(as, (_: Int, _: Int) => true) should be (true)
  }

  "isSorted" should "correctly detect sorted arrays" in {
    val as = Array(1,2,4,4,5)
    Ch2.isSorted(as, (a: Int, b: Int) => a<=b) should be (true)
  }

}
