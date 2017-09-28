package fpclub5

import org.scalatest.{FlatSpec, Matchers}

class Ch5Spec extends FlatSpec with Matchers {

  "Stream.take" should "work for n < size of Stream" in {
    Stream(1,2,3).take(2).toList should be(List(1,2))
    Stream(1,2,3,4,5,6,7).take(4).toList should be(List(1,2,3,4))
  }
  "Stream.takeAsUnfold" should "work for n < size of Stream" in {
    Stream(1,2,3).takeAsUnfold(2).toList should be(List(1,2))
    Stream(1,2,3,4,5,6,7).takeAsUnfold(4).toList should be(List(1,2,3,4))
  }
  "Stream.take" should "return Stream for n == size of Stream" in {
    val s_4 = Stream(1,2,3,4)
    val s_14 = Stream(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
    s_4.take(4).toList should be(s_4.toList)
    s_14.take(14).toList should be(s_14.toList)
  }
  "Stream.takeAsUnfold" should "return Stream for n == size of Stream" in {
    val s_4 = Stream(1,2,3,4)
    val s_14 = Stream(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
    s_4.takeAsUnfold(4).toList should be(s_4.toList)
    s_14.takeAsUnfold(14).toList should be(s_14.toList)
  }
  "Stream.take" should "return Stream for n > size of Stream" in {
    Stream(1,2,3).take(22).toList should be(List(1,2,3))
    Stream(1,2,3).take(12).toList should be(List(1,2,3))
  }
  "Stream.takeAsUnfold" should "return Stream for n > size of Stream" in {
    Stream(1,2,3).takeAsUnfold(22).toList should be(List(1,2,3))
    Stream(1,2,3).takeAsUnfold(12).toList should be(List(1,2,3))
  }
  "Stream.take" should "return Empty on empty Stream" in {
    Stream.empty.take(8) should be(Empty)
  }
  "Stream.takeAsUnfold" should "return Empty on empty Stream" in {
    Stream.empty.takeAsUnfold(8) should be(Empty)
  }
  "Stream.take" should "return Empty for n <= 0" in {
    val s = Stream(1,2,3,4)
    s.take(0) should be(Empty)
    s.take(-2) should be(Empty)
    s.take(-9) should be(Empty)
  }
  "Stream.takeAsUnfold" should "return Empty for n <= 0" in {
    val s = Stream(1,2,3,4)
    s.takeAsUnfold(0) should be(Empty)
    s.takeAsUnfold(-2) should be(Empty)
    s.takeAsUnfold(-9) should be(Empty)
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
  "Stream.takeWhileAsUnfold" should "just work" in {
    Stream(1,4,2,-4,0,3,5,1,5).takeWhileAsUnfold(a => a>0).toList should be(List(1,4,2))
  }

  "Stream.takeWhile" should "only evaluate thunks once" in {
    var count = 0
    val expensiveZero = { println("**expensive computation**"); count+=1; 0 }
    val s = Stream.cons(expensiveZero, Stream(1, 2, 3, -5, 4))
    println("created Stream")
    s.takeWhile(_ >= 0).toList should be(List(0,1,2,3))
    count should be(1)
  }
  "Stream.takeWhileAsUnfold" should "only evaluate thunks once" in {
    var count = 0
    val expensiveZero = { println("**expensive computation**"); count+=1; 0 }
    val s = Stream.cons(expensiveZero, Stream(1, 2, 3, -5, 4))
    println("created Stream")
    s.takeWhileAsUnfold(_ >= 0).toList should be(List(0,1,2,3))
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

  "Stream.map" should "just work" in {
    Stream(1,2,3).map(_+1).toList should be(List(2,3,4))
  }
  "Stream.mapAsUnfold" should "just work" in {
    Stream(1,2,3).mapAsUnfold(_+1).toList should be(List(2,3,4))
  }

  "Stream.filter" should "just work" in {
    Stream(1,-1,2,-2,3,-3,4,-4).filter(_>0).toList should be(List(1,2,3,4))
  }

  "Stream.append" should "just work" in {
    Stream(1,2,3).append(Empty).toList should be(List(1,2,3))
    Empty.append(Stream(1,2,3)).toList should be(List(1,2,3))
    Empty.append(Empty) should be(Empty)
    Stream(4,4).append(Stream(1,2,3,4)).toList should be(List(4,4,1,2,3,4))
  }

  "Stream.flatMap" should "just work" in {
    Stream(1,2).flatMap(a => Stream(a,a)).toList should be (List(1,1,2,2))
    Stream(0,1,2).flatMap(a => if (a>0) Stream(a,a) else Empty).toList should be (List(1,1,2,2))
  }

  "Stream.constant" should "just work" in {
    Stream.constant("ja").take(4).toList should be(List("ja", "ja", "ja", "ja"))
    Stream.constant(9).takeWhile(_<0) should be(Empty)
  }
  "Stream.constantAsUnfold" should "just work" in {
    Stream.constantAsUnfold("ja").take(4).toList should be(List("ja", "ja", "ja", "ja"))
    Stream.constantAsUnfold(9).takeWhile(_<0) should be(Empty)
  }

  "Stream.from" should "just work" in {
    Stream.from(99).take(5).toList should be(List(99,100,101,102,103))
  }
  "Stream.fromAsUnfold" should "just work" in {
    Stream.fromAsUnfold(99).take(5).toList should be(List(99,100,101,102,103))
  }

  "Stream.fibs" should "just work" in {
    Stream.fibs.take(6).toList should be(List(0,1,1,2,3,5))
  }
  "Stream.fibsAsUnfold" should "just work" in {
    Stream.fibsAsUnfold.take(6).toList should be(List(0,1,1,2,3,5))
  }

  "Stream.unfold" should "just work" in {
    def Try[A](a: =>A): Option[A] = {
      try Some(a)
      catch { case _: Exception => None }
    }
    def f(s: String): Option[(Int, String)] = {
      Try(Integer.parseInt(s)).map(i => (i, s+"0"))
    }
    Stream.unfold("1")(f).take(4).toList should be(List(1,10,100,1000))
  }

  "Stream.zipWithAsUnfold" should "just work" in {
    val s = Stream(1,2,3)
    s.zipWithAsUnfold(Stream(3,3,9))(_+_).toList should be(List(4,5,12))

    s.zipWithAsUnfold(Stream(3,3,9,1,3,45))(_+_).toList should be(List(4,5,12))

    s.zipWithAsUnfold(Stream(1,1))(_+_).toList should be(List(2,3))

    s.zipWithAsUnfold(Empty)(_+_) should be(Empty)
    Empty.zipWithAsUnfold(Empty)((a,_)=>a) should be(Empty)
    Empty.zipWithAsUnfold(s)((a,_)=>a) should be(Empty)
  }

  "Stream.zipAll" should "just work" in {
    Stream(1,2).zipAll(Stream(3,9)).toList should be(
      List(
        (Some(1),Some(3)),
        (Some(2),Some(9))
      )
    )

    Stream(1,2).zipAll(Stream(3,3,9)).toList should be(
      List(
        (Some(1),Some(3)),
        (Some(2),Some(3)),
        (None,Some(9))
      )
    )

    Empty.zipAll(Empty) should be(Empty)

    Stream("a","b").zipAll(Empty).toList should be(
      List(
        (Some("a"), None),
        (Some("b"), None)
      )
    )
  }

  "Stream.startsWith" should "just work" in {
    Stream(1,2,3).startsWith(Stream(1)) should be(true)
    Stream(1,2,3).startsWith(Stream(1,2)) should be(true)
    Stream(1,2,3).startsWith(Stream(1,2,3)) should be(true)

    Stream(1,2,3).startsWith(Stream(1,4)) should be(false)

    Stream(1,2,3).startsWith(Stream(1,2,3,4)) should be(false)

    Stream(1,2,3).startsWith(Empty) should be(true)

    Empty.startsWith(Empty) should be(true)
  }

  "Stream.tails" should "just work" in {
    Stream(1,2,3).tails.toList.map(s => s.toList) should be(
      List(
        List(1,2,3),
        List(2,3),
        List(3)
      )
    )
  }

  "Stream.scanRight" should "just work" in {
    Stream(1,2,3,4).scanRight(55)(_+_).toList should be(
      List(
        1+2+3+4+55,
        2+3+4+55,
        3+4+55,
        4+55
      )
    )
  }
}
