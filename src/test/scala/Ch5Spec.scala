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

  "Stream.takeWhile" should "only evaluate thunks once" in {
    var count = 0
    val expensiveZero = { println("**expensive computation**"); count+=1; 0 }
    val s = Stream.cons(expensiveZero, Stream(1, 2, 3, -5, 4))
    println("created Stream")
    s.takeWhile(_ >= 0).toList should be(List(0,1,2,3))
    count should be(1)
  }

  "Stream.forAll" should "correctly determine true" in {
    val s = Stream(1,2,3,4,5,6,7,8)
    s.forAll(_>0) should be(true)
  }

  "Stream.forAll" should "be able to terminate early" in {
    var count = 0
    val s = Stream(1,2,3,-4,5,6,7,8)
    s.forAll(a => {count+=1; a>0}) should be(false)
    count should be(4)
  }

  "headOption" should "return None for empty Stream" in {
    Stream().headOption should be(None)
  }

  "headOption" should "return Some(a) for Stream(a,b,c)" in {
    Stream(4,5,6).headOption should be(Some(4))
  }

  "headOption" should "not evaluate tail of Stream" in {
    var count = 0;
    val s = Stream.cons(
      {count +=1; 1},
      Stream.cons(
        {count +=1; 2},
        Stream.cons(
          {count +=1; 3},
          Stream.cons(
            {count +=1; 4},
            Empty)
        )
      )
    )
    s.headOption should be(Some(1))
    count should be(1)
  }
}
