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

sealed trait Either[+E,+A] {
  // ex 4.6
  def map[B](f: A=>B): Either[E,B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
  }
  def flatMap[EE>:E,B](f: A=>Either[EE,B]): Either[EE,B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }
  def orElse[EE>:E,B>:A](b: =>Either[EE,B]): Either[EE,B] = {
    this match {
      case Left(_) => b
      case _ => this
    }
  }
  def map2[EE>:E,B,C](b: Either[EE,B])(f: (A,B)=>C): Either[EE,C] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => b.map(b => f(a,b))
    }
  }
}
case class Left[+E](e:E) extends Either[E,Nothing]
case class Right[+A](a:A) extends Either[Nothing,A]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def Try[A](a: =>A): Option[A] = {
    try Some(a)
    catch { case e: Exception => None }
  }

  // ex 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => {
      val vs = xs.map(x => math.pow(x-m, 2))
      mean(vs)
    })
  }

  // ex 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B)=>C): Option[C] = {
    a.flatMap(aa => {
      b.map(bb => f(aa,bb))
    })
    // above using flatmaps
    // below using for-comprehension *syntactic sugar*
    for {
      aa <- a
      bb <- b
    } yield f(aa,bb)
  }

  // ex 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
//    as.foldRight(Some(Nil): Option[List[A]])((a, acc) => {
//      (a,acc) match {
//        case (None, _) => None
//        case (_, None) => None
//        case (Some(x), Some(xs)) => Some(x::xs)
//      }
//    })
    // reimplemented in terms of traverse
    traverse(as)(a => a)
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
}


object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty) Left("empty list!")
    else Right(xs.sum / xs.length)
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try Right(x / y)
    catch { case e:Exception => Left(e) }
  }

  def Try[A](a: => A): Either[Exception,A] = {
    try Right(a)
    catch { case e:Exception => Left(e) }
  }

  // ex 4.7
  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
    es.foldRight(Right(Nil):Either[E,List[A]])((e, acc) => {
      (e,acc) match {
        case (Left(x),_) => Left(x) // this must be first so we always return the first error (since this is foldR)
        case (_,Left(x)) => Left(x)
        case (Right(x), Right(xs)) => Right(x::xs)
      }
    })
  }
  def traverse[E,A,B](as: List[A])(f: A=>Either[E,B]): Either[E,List[B]] = {
    as.foldRight(Right(Nil):Either[E,List[B]])((a,acc) => {
      (f(a),acc) match {
        case (Left(e),_) => Left(e)
        case (_,Left(e)) => Left(e)
        case (Right(b),Right(bs)) => Right(b::bs)
      }
    })
  }

  // ex 4.8: i would change map2 to accumulate the error strings (by concat'ing them)
  //
  // this could be generalized by having the concat function be customizable, which
  // would necessitate the introduction of a more-general contain type (than `Either`)
  //
  // alternatively, we could use an `Either[List[String], A]`
}
