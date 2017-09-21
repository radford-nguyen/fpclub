package fpclub5

import org.scalatest.{FlatSpec, Matchers}

class Ch5Spec extends FlatSpec with Matchers {

  "Stream.take" should "work for n < size of Stream" in {
    Stream(1,2,3).take(2).toList should be(List(1,2))
    Stream(1,2,3,4,5,6,7).take(4).toList should be(List(1,2,3,4))
  }
  "Stream.take" should "return Stream for n == size of Stream" in {
    val s_4 = Stream(1,2,3,4)
    val s_14 = Stream(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
    s_4.take(4).toList should be(s_4.toList)
    s_14.take(14).toList should be(s_14.toList)
  }
  "Stream.take" should "return Stream for n > size of Stream" in {
    Stream(1,2,3).take(22).toList should be(List(1,2,3))
    Stream(1,2,3).take(12).toList should be(List(1,2,3))
  }
  "Stream.take" should "return Empty on empty Stream" in {
    Stream.empty.take(8) should be(Empty)
  }
  "Stream.take" should "return Empty for n <= 0" in {
    val s = Stream(1,2,3,4)
    s.take(0) should be(Empty)
    s.take(-2) should be(Empty)
    s.take(-9) should be(Empty)
  }

  "Stream.drop" should "work for 0 < n < size of Stream" in {
    Stream(1,2,3).drop(1).toList should be(List(2,3))
    Stream(1,2,3,4,5,6,7).drop(4).toList should be(List(5,6,7))
  }
  "Stream.drop" should "return Empty for n > size of Stream" in {
    Stream(1,2,3).drop(23) should be(Empty)
    Stream(1,3).drop(23) should be(Empty)
    Stream(1,2,3,4,5,6,7,8,9,10,11,12,13).drop(23) should be(Empty)
  }

  "Stream.takeWhile" should "just work" in {
    Stream(1,4,2,-4,0,3,5,1,5).takeWhile(a => a>0).toList should be(List(1,4,2))
  }
}
