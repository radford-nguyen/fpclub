package fpclub5

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h()) // evaluate head (but not tail)
  }
  // ex 5.1
  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, t) => h()::t().toList
    }
  }
  // ex 5.2
  def take(n: Int): Stream[A] = {
    this match {
      case Empty => Empty
      case _ if n <=0 => Empty
      case Cons(h, t) => Stream.cons(h(), t().take(n-1))
    }
  }
  @annotation.tailrec
  final def drop(n :Int): Stream[A] = {
    (n,this) match {
      case (_,Empty) => Empty
      case (0,_) => this
      case (n,Cons(_, t)) => t().drop(n-1)
    }
  }
  // ex 5.3
  def takeWhile(p: A=>Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) => {
        lazy val hd = h()
        if (p(hd))
          Stream.cons(hd, t().takeWhile(p))
        else
          Empty
      }
    }
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
