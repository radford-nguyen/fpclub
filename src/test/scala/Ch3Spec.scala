package fpclub
import org.scalatest.{FlatSpec, Matchers}

class Ch3Spec extends FlatSpec with Matchers {

  "length" should "work for Lists of various types and lengths" in {
    Ch3.length(List(4,1,6,3)) should be (4);
    Ch3.length(List("hello", "world")) should be (2);
  }

  "foldL" should "work correctly :)" in {
    Ch3.foldL(List(1,4,5), 0)(_+_) should be (0+1+4+5)
  }

  "reverse" should "work when implemented with foldR" in {
    Ch3.reverse(List(1,2,3,4)) should be (List(4,3,2,1))
  }

  "foldLinR" should "work somehow magically" in {
    Ch3.foldLinR(List(1,4,5), 0)(_+_) should be (0+1+4+5)
  }

  "foldRinL" should "work somehow magically" in {
    Ch3.foldRinL(List(1,4,5), Nil:List[Int])(Cons(_,_)) should be(List(1,4,5))
  }

  "append" should "work" in {
    Ch3.append(List(3,1), List(5,1,54,2)) should be(List(3,1,5,1,54,2))
  }

  "concat" should "work" in {
    Ch3.concat(List(List(1,2), List(3), List(4,5,6), List(7,8,9,10))) should be(List(1,2,3,4,5,6,7,8,9,10))
  }

  "test case for ex 3.19" should "remove odd numbers from List[Int]" in {
    def ints = List(1,2,3,4,5,6,7)
    def isEven = (i:Int) => i%2 == 0
    Ch3.filter(ints)(isEven) should be(List(2,4,6))
  }

  "test case for ex 3.20" should "work correctly" in {
    Ch3.flatMap(List(1,2,3))(i => List(i,i)) should be(List(1,1,2,2,3,3))
  }

  "flatFilter" should "work same as filter" in {
    def ints = List(1,2,3,4,5,6,7)
    def isEven = (i:Int) => i%2 == 0
    Ch3.filter(ints)(isEven) should be(List(2,4,6))
    Ch3.flatFilter(ints)(isEven) should be(List(2,4,6))
  }


}
