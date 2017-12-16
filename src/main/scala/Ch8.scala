package fpclub8

import fpclub6.Ch6
import fpclub6.Ch6.{RNG, State}


object Ch8 {

  // ex 8.1
  /**
    * Properties for `sum: List[Int] => Int`
    *
    * - sum(Nil) == 0
    * - sum(l) == x * size(l) | for all i in [0,size(l)), l(i) == x
    * - sum(l) == sum(reverse(l))
    * - sum(l.append(m)) == sum(l) + sum(m)
    */

  // ex 8.2
  /**
    * Properties for `max: List[Int] => Int`
    *
    * - max(Nil) == Int.MIN_VALUE
    * - max(a::b::Nil) == a | for all a,b where a >= b
    * - max(l) == l.head | size(l) == 1
    * - max(l) == x | for all i in [0,size(l)), l(i) == x
    * - max(l) == max(reverse(l))
    * - max(l.append(m)) == max(max(l)::max(m)::Nil)
    */

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  object Prop {
    // the number of tests that succeeded
    type SuccessCount = Int

    // information about the failing testcase
    type FailedCase = String
  }
  trait Prop {
    def check: Either[(Prop.FailedCase, Prop.SuccessCount), Prop.SuccessCount]

    // ex 8.3
//    def &&(p: Prop): Prop = {
//      val b = this.check
//      new Prop {
//        def check = b && p.check
//      }
//    }
  }

  case class Gen[A](sample: Ch6.State[Ch6.RNG,A]) {
    def map[B](f:A=>B): Gen[B] = {
      flatMap(a => Gen.unit(f(a)))
    }
    // ex 8.6
    def flatMap[A,B](f:A=>Gen[B]): Gen[B] = {
      Gen(Ch6.State(rng => {
        val (b: Gen[B],r: RNG) = sample.map(f).run(rng)
        b.sample.run(r)
      }))
    }
    def listOfN[A](size: Gen[Int]): Gen[List[A]] = {
      for {
        n <- size
        as <- Gen.listOfN(n, this)
      } yield as
    }
  }
  object Gen {
    // ex 8.4
    def choose(start: Int, stopx: Int): Gen[Int] = {
      def randRange = Ch6.State(Ch6.nonNegativeLessThan(stopx-start)).map(start + _)
      Gen(randRange)
    }

    // ex 8.5
    /**
      * Always generates the value `a`
      *
      * @param a
      * @tparam A
      * @return
      */
    def unit[A](a: => A): Gen[A] = Gen(Ch6.State(Ch6.unit(a)))

    def boolean: Gen[Boolean] = Gen(Ch6.State(Ch6.int).map(_%2 == 0))

    /**
      * Generates lists of length `n` using `g`
      *
      * @param n
      * @param g
      * @tparam A
      * @return
      */
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      Gen(Ch6.State(rng => {
        def go(n: Int, r: RNG, as: List[A]): (List[A],RNG) = {
          n match {
            case n if n <= 0 => (as,r)
            case n => {
              val (a, rng2) = g.sample.run(r)
              go(n-1, rng2, a::as)
            }
          }
        }
        go(n, rng, List())
      }))
    }
  }
}

