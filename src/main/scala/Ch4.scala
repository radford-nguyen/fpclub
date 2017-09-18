package fpclub4

import scala.{Option => _, Either => _}

sealed trait Option[+A] {
  def map[B](f: A=>B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  def getOrElse[B>:A](default: =>B): B =
    this match {
      case None => default
      case Some(a) => a
    }
  def flatMap[B](f: A=>Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }
  def orElse[B>:A](ob: =>Option[B]): Option[B] = {
    map(a=>Some(a)).getOrElse(ob)
  }
  def filter(p: A=>Boolean): Option[A] = {
    flatMap(a => if (p(a)) Some(a) else None)
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Ch4 {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  // ex 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => {
      val vs = xs.map(x => math.pow(x-m, 2))
      mean(vs)
    })
  }

  def Try[A](a: =>A): Option[A] = {
    try Some(a)
    catch { case e: Exception => None }
  }

  // ex 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B)=>C): Option[C] = {
    a.flatMap(a => {
      b.map(b => f(a,b))
    })
  }

  // ex 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    as.foldRight(Some(Nil): Option[List[A]])((a, acc) => {
      (a,acc) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(x), Some(xs)) => Some(x::xs)
      }
    })
  }

  // ex 4.5
  def traverse[A,B](as: List[A])(f: A=>Option[B]): Option[List[B]] = {
    as.foldRight(Some(Nil):Option[List[B]])((a,acc) => {
      (f(a),acc) match {
        case (None,_) => None
        case (_,None) => None
        case (Some(b), Some(bs)) => Some(b::bs)
      }
    })
  }
  def sequenceT[A](as: List[Option[A]]): Option[List[A]] = {
    traverse(as)(a => a)
  }
}