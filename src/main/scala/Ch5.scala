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
//    this match {
//      case Cons(h, t) => p(h()) || t().exists(p)
//      case _ => false
//    }
    // in terms of foldRight
    //
    // note that the solution in terms of foldRight
    // is not stack-safe like the original
    foldRight(false)((a,b) => p(a) || b)
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
}
