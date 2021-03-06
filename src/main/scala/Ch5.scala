package fpclub5

sealed trait Stream[+A] {
  def headOption: Option[A] = {
//    this match {
//      case Empty => None
//      case Cons(h, _) => Some(h()) // evaluate head (but not tail)
//    }
    // ex 5.6: in terms of foldRight
    foldRight(None:Option[A])((a,_) => Some(a))
  }
  // ex 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h()::t().toList
  }
  // ex 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n>0 => Stream.cons(h(), t().take(n-1))
    case _ => Empty
  }
  @annotation.tailrec
  final def drop(n :Int): Stream[A] = this match {
    case Empty => Empty
    case _ if n==0 => this
    case Cons(_, t) => t().drop(n-1)
  }
  // ex 5.3
  def takeWhile(p: A=>Boolean): Stream[A] = {
//    this match {
//      case Cons(h, t) if (p(h())) => Stream.cons(h(), t().takeWhile(p))
//      case _ => Empty
//    }
    // ex 5.5: in terms of foldRight
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else Empty)
  }

  def exists(p: A=>Boolean): Boolean = {
    this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }
    // in terms of foldRight
    //
    // note that the solution in terms of foldRight
    // is not stack-safe like the original
//    foldRight(false)((a,b) => p(a) || b)
  }

  def foldRight[B](z: =>B)(f: (A, =>B) => B): B = this match {
    case Empty => z
      // notice that the 2nd arg to f is by-name.
      // if f doesn't use the 2nd arg, then the
      // recursion never occurs (meaning traversal terminates early)
      //
      // notice the similarity b/w this and the
      // use of || in exists(A=>Boolean)
    case Cons(h,t) => f( h(), t().foldRight(z)(f) )
  }

  // ex 5.4
  def forAll(p: A=>Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  // ex 5.5: see takeWhile above

  // ex 5.6: see headOption above

  // ex 5.7
  def map[B](f: A=>B): Stream[B] = {
    foldRight(Empty:Stream[B])((a,b) => Stream.cons(f(a), b))
  }
  def filter(p: A=>Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)
  }
  def append[AA>:A](s: =>Stream[AA]): Stream[AA] = {
    foldRight(s)(Stream.cons(_,_))
  }
  def flatMap[B](f: A=>Stream[B]): Stream[B] = {
    foldRight(Empty:Stream[B])((a,b) => f(a).append(b))
  }

  // ex 5.13
  def mapAsUnfold[B](f: A=>B): Stream[B] = {
    Stream.unfold(this)(s => {
      s match {
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
      }
    })
  }
  def takeAsUnfold(n: Int): Stream[A] = {
    Stream.unfold((this,n))(s => {
      s match {
        case (Cons(h,t), x) if x > 0 => Some((h(), (t(), x-1)))
        case _ => None
      }
    })
  }
  def takeWhileAsUnfold(p: A=>Boolean): Stream[A] = {
    Stream.unfold(this)(s => {
      s match {
        case Cons(h,t) if (p apply h()) => Some((h(), t()))
        case _ => None
      }
    })
  }
  def zipWithAsUnfold[B,C](bs: Stream[B])(f: (A,B)=>C): Stream[C] = {
    Stream.unfold((this, bs))(s => {
      s match {
        case (Empty, _) => None
        case (_, Empty) => None
        case (Cons(a, atail), Cons(b, btail)) => Some((f(a(), b()),(atail(),btail())))
      }
    })
  }
  def zipAll[B](bs: Stream[B]): Stream[(Option[A],Option[B])] = {
    Stream.unfold((this, bs))(s => {
      s match {
        case (Cons(a, atail), Empty) => Some(( (Some(a()),None), (atail(),Empty) ))
        case (Empty, Cons(b, btail)) => Some(( (None,Some(b())), (Empty,btail()) ))
        case (Cons(a, atail), Cons(b, btail)) => Some(( (Some(a()),Some(b())), (atail(),btail()) ))
        case (Empty, Empty) => None
      }
    })
  }

  // ex 5.14
  def startsWith[A](sub: Stream[A]): Boolean = {
    // create a (lazy) stream of bools that
    // indicate if `as` and `bs` have the same head
    val sameElems: Stream[Boolean] = Stream.unfold((this, sub))(s => {
      s match {
        case (_, Empty) => None // if the subsequence is empty, we're done
        case (Empty, _) => Some(false, (Empty,Empty))
        case (Cons(a, atail), Cons(b, btail)) => Some(a()==b(), (atail(),btail()))
      }
    })

    // then, simply traverse that lazy stream
    // until either `false` (in which case return false)
    // or `Empty` (in which case return true)
    sameElems.forAll(p => p)
  }

  // ex 5.15
  def tails: Stream[Stream[A]] = {
    var end = false
    Stream.unfold(this)(s => {
      s match {
        case Empty if end => None
        case Empty => end=true; Some(s, Empty)
        case Cons(_, t) => Some(s, t())
      }
    })
  }

  def hasSubsequence[A](sub: Stream[A]): Boolean = {
    tails exists (_ startsWith sub)
  }

  // ex 5.16
  def scanRight[B](z:B)(f: (A,B)=>B): Stream[B] = {
    foldRight(Empty:Stream[B])((a,bs) => {
      bs match {
        case Empty => Stream(f(a,z), z) // first result
        case Cons(b, _) => Stream.cons(f(a,b()), bs)
      }
    })
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: =>A, t: =>Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(()=>head, ()=>tail)
  }
  def empty[A](): Stream[A] = Empty
  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty)
      empty()
    else
      cons(as.head, apply(as.tail: _*))
  }
  val ones: Stream[Int] = constant(1)

  // ex 5.8
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  // more efficient constant: an object that just references itself
  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // ex 5.9
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  // ex 5.10
  val fibs: Stream[Int] = {
    def f(a: Int, b: Int):Stream[Int] = {
      Stream.cons(a, f(b, b+a))
    }
    f(0,1)
  }

  // ex 5.11
  def unfold[A,S](z: S)(f: S=>Option[(A,S)]): Stream[A] = {
    f(z) match {
      case None => Empty: Stream[A]
      case Some((a,s)) => Stream.cons(a, unfold(s)(f))
    }
  }

  // ex 5.12
  val fibsAsUnfold: Stream[Int] = {
    unfold((0,1))(t => {
      Some((t._1, (t._2, t._1+t._2)))
    })
  }
  def fromAsUnfold(n: Int): Stream[Int] = {
    unfold(n)(i => Some((i, i+1)))
  }
  def constantAsUnfold[A](a: A): Stream[A] = {
    unfold(a)(_ => Some(a, a))
  }
  val onesAsUnfold: Stream[Int] = {
    unfold(1)(_ => Some(1,1))
  }

}
