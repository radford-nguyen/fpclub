package fpclub6

object Ch6 {

  // this is a type alias
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](rand: Rand[A])(f: A=>B): Rand[B] =
    rng => {
      val (a,r1) = rand(rng)
      (f(a), r1)
    }

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRng = SimpleRNG(newSeed)
      val n = (newSeed >> 16).toInt
      (n, nextRng)
    }
  }

  // ex 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (i, r) if i == Int.MinValue => nonNegativeInt(r)
      case (i, r) if i < 0 => (i * -1, r)
      case (i, r) => (i, r)
    }
  }

  // ex 6.2
  /**
    * Generates a random `Double` between [0, 1)
    *
    * @param rng
    * @return
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val d = i / (Int.MaxValue.toDouble + 1)
    (d, r)
  }

  // ex 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, nextR) = double(r)
    ((i, d), nextR)
  }
  def intDouble2: Rand[(Int,Double)] = rng => {
    val (i, r) = rng.nextInt
    val (d, nextR) = double(r)
    ((i, d), nextR)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i%2)

  // ex 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(n:Int, acc:List[Int])(r:RNG): (List[Int],RNG) = {
      n match {
        case n if n <= 0 => (acc,r)
        case n => {
          val (i,r1) = r.nextInt
          go(n-1, i::acc)(r1)
        }
      }
    }
    go(count, List())(rng)
  }

  // ex 6.5
  def doubleAsMap: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // ex 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B)=>C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a,b), r2)
  }

  def both[A,B](ra:Rand[A])(rb:Rand[B]): Rand[(A,B)] =
    map2(ra,rb)((_,_))

  val randIntDouble: Rand[(Int,Double)] =
    both(_.nextInt)(double)

  val ranDoubleInt: Rand[(Double,Int)] =
    both(double)(_.nextInt)

  // ex 6.7
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = {
    def init:Rand[List[A]] = unit(Nil:List[A])
    rs.foldRight(init)((ra, randList) => {
      map2(ra, randList)((a,as) => a::as)
    })
  }
  def ints2(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  // ex 6.8
  def flatMap[A,B](r: Rand[A])(f: A=>Rand[B]): Rand[B] = {
    rng => {
      val (a, r2) = r(rng)
      f(a)(r2)
    }
  }
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }

  // ex 6.9
  def mapAsFlatMap[A,B](rand: Rand[A])(f: A=>B): Rand[B] = {
    flatMap(rand)(a => unit(f(a)))
  }
  def map2AsFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B)=>C): Rand[C] = {
    flatMap(ra)(a => {
      flatMap(rb)(b => {
        unit(f(a,b))
      })
    })
  }

  // ex 6.10
  case class State[S,+A](run: S => (A,S)) {
    def map[B](f: A=>B): State[S,B] =
      flatMap(a => State.unit(f(a)))

    def map2[B,C](rb: State[S,B])(f: (A,B)=>C): State[S,C] =
      flatMap(a => {
        flatMap(b => {
          State.unit(f(a,b))
        })
      })

    def flatMap[B](f: A=>State[S,B]): State[S,B] = State(s => {
      val (a,r2) = run(s)
      f(a).run(r2)
    })
  }
  object State {
    def unit[A,S](a:A): State[S,A] = State(s => (a,s))
    def sequence[A,S](ss: List[State[S,A]]): State[S,List[A]] = {
      def init:State[S,List[A]] = unit(Nil:List[A])
      ss.foldRight(init)((s, stateAs) => {
        s.map2(stateAs)((a,as) => a::as)
      })
    }
  }

}
