package fpclub

import org.scalatest.{FlatSpec, Matchers}
import scala.{Option => _, Either => _}

class Ch4Spec extends FlatSpec with Matchers {

  "Option.flatMap" should "just work" in {
    val root:Double=>Option[Double] = i =>
      if (i >= 0) Some(math.sqrt(i))
      else None

    Some(4.0).flatMap(root) should be(Some(2.0))
    Some(-4.0).flatMap(root) should be(None)
    None.flatMap(root) should be(None)
  }

  "map2" should "just work" in {
    val f: (Int, String) => Int = (i,s) => i + Integer.parseInt(s)
    Ch4.map2(Some(4), Some("3"))(f) should be(Some(7))
    Ch4.map2(Some(4), None)(f) should be(None)
    Ch4.map2(None, Some("3"))(f) should be(None)
    Ch4.map2(None, None)(f) should be(None)
  }
}
