package fpclub4

import org.scalatest.{FlatSpec, Matchers}

import scala.{Either => _, Option => _}

class Ch4Spec extends FlatSpec with Matchers {

  "Option.flatMap" should "just work" in {
    val root:Double=>Option[Double] = i =>
      if (i >= 0) Some(math.sqrt(i))
      else None

    Some(4.0).flatMap(root) should be(Some(2.0))
    Some(-4.0).flatMap(root) should be(None)
    None.flatMap(root) should be(None)
  }

  "Option.map2" should "just work" in {
    val f: (Int, String) => Int = (i,s) => i + Integer.parseInt(s)
    Option.map2(Some(4), Some("3"))(f) should be(Some(7))
    Option.map2(Some(4), None)(f) should be(None)
    Option.map2(None, Some("3"))(f) should be(None)
    Option.map2(None, None)(f) should be(None)
  }

  "Option.sequence" should "just work" in {
    val xs = Some(1)::Some(2)::None::Some(4)::Nil
    val ys = Some(1)::Some(2)::Some(3)::Some(4)::Nil
    Option.sequence(xs) should be(None)
    Option.sequence(ys) should be(Some(1::2::3::4::Nil))
  }

  "Option.traverse" should "just work" in {
    val f: String => Option[Int] = i => Option.Try(i.toInt)

    Option.traverse("1"::"2"::"3"::"4"::Nil)(f) should be(Some(1::2::3::4::Nil))
    Option.traverse("1"::"2"::"blorp"::"4"::Nil)(f) should be(None)
  }

  "Either.sequence" should "return the first error when multiple errors are present" in {
    val es = Right(1)::Left("null")::Left("foobar")::Left("empty")::Nil
    Either.sequence(es) should be(Left("null"))
  }

  "Either.traverse" should "just work" in {
    val f: String => Either[String, Int] = s => {
      try Right(s.toInt)
      catch { case _:Exception => Left(s) }
    }

    Either.traverse("1"::"2"::"3"::"4"::Nil)(f) should be(Right(1::2::3::4::Nil))
    Either.traverse("1"::"2"::"blorp"::"4"::Nil)(f) should be(Left("blorp"))
    Either.traverse("1"::"blep"::"blorp"::"4"::Nil)(f) should be(Left("blep"))
  }

}
