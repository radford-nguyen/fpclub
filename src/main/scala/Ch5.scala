package fpclub5

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h()) // evaluate head (but not tail)
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
  def takeWhile(p: A=>Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def exists(p: A=>Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
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
