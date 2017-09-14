package fpclub

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](a:A, as:List[A]) extends List[A]
object List {
  def sum(ints:List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(i, is) => i + sum(is)
    }
  }
  def product(ints:List[Double]): Double = {
    ints match {
      case Nil => 1.0
      case Cons(0, _) => 0.0 // quit eagerly
      case Cons(i, is) => i * product(is)
    }
  }
  def apply[A](as: A*): List[A] = {
    if (as.length == 0) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}

object Ch3 {
  def head[A](xs:List[A]): A = {
    xs match {
      case Nil => sys.error("empty list")
      case Cons(x, _) => x
    }
  }

  // ex 3.2
  def tail[A](as:List[A]): List[A] = {
    as match {
      case Nil => Nil
      case Cons(_, as) => as
    }
  }
  // ex 3.3
  def setHead[A](as:List[A], newHead:A): List[A] = {
    as match {
      case Nil => Nil
      case Cons(_, as) => Cons(newHead, as)
    }
  }
  // ex 3.4
  @annotation.tailrec
  def drop[A](n:Int, as:List[A]): List[A] = {
    as match {
      case Nil => Nil
      case _ if (n <= 0) => as
      case Cons(_, tail) => drop(n-1, tail)
    }
  }
  // ex 3.5
  def dropWhile[A](p:A=>Boolean)(as:List[A]): List[A] = {
    as match {
      case Nil => Nil
      case Cons(h, tail) if (p(h)) => dropWhile(p)(tail)
      case _ => as
    }
  }
  // ex 3.6
  def init[A](as:List[A]): List[A] = {
    as match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, tail) => Cons(h, init(tail))
    }
  }

  def foldR[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    // compare structure of this function to
    // our previous impl of `sum`:
//    def sum(ints:List[Int]): Int = {
//      val f = (_+_)
//      ints match {
//        case Nil => 0
//        case Cons(i, is) => f(i, sum(is))
//      }
//    }
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldR(xs, z)(f))
    }
  }

  // ex 3.7: NO

  // ex 3.8: You get the original List back.  It suggests that
  // `Cons` represents the same recursive structure as foldR,
  // with `Nil` being the R value

  // ex 3.9
  def length[A](as: List[A]): Int = {
    foldR(as, 0)((_,y)=>y+1)
  }

  // ex 3.10
  @annotation.tailrec
  def foldL[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldL(xs, f(z, x))(f)
    }
  }

  // ex 3.11
  def sumL(xs: List[Int]): Int = {
    foldL(xs, 0)(_+_)
  }
  def productL(xs:List[Double]): Double = {
    foldL(xs, 1.0)(_*_)
  }
  def lenL[A](xs:List[A]): Int = {
    foldL(xs, 0)((y,_) => 1+y)
  }

  // ex 3.12
  def reverse[A](as: List[A]): List[A] =  {
    foldL(as, Nil:List[A])((acc, x) => Cons(x, acc))
  }

  // ex 3.13
  //
  // Notes:
  //
  // We get a glimpse of how this works (and why the 2
  // implementations are so similar) by thinking about
  // function composition.
  //
  // Composing functions `f.g.h` in some sense "reverses"
  // the order of evaluation to h,g,f.  Put another way,
  // composition turns the evaluation inside-out.
  //
  // This is why function composition can be used to turn
  // a fold inside-out.
  //
  // For a better/deeper/more-technical explanation, google
  // "universal property of folds"
  def foldLinR[A,B](as: List[A], b:B)(f: (B,A) => B): B = {
    def id[T](x:T): T = x
    def stepR(x:A, g: B => B): (B=>B) = {
      acc => g(f(acc, x))
    }
    val accF:(B=>B) = foldR(as, id:B=>B)(stepR)
    accF(b)
  }
  def foldRinL[A,B](as: List[A], b:B)(f: (A,B) => B): B = {
    def stepL(g: B=>B, a:A): (B=>B) = {
      x => g(f(a, x))
    }
    val accF = foldL(as, (x=>x):B=>B)(stepL): (B=>B)
    accF(b)
  }

  // ex 3.14
  def append[A](as: List[A], bs: List[A]): List[A] = {
    foldR(as, bs)(Cons(_,_))
  }

  // ex 3.15
  def concat[A](as: List[List[A]]): List[A] = {
    foldL(as, Nil:List[A])(append)
  }

  // ex 3.16
  def incrEach(is: List[Int]): List[Int] = {
    foldR(is, Nil:List[Int])((a,b) => Cons(a+1, b))
  }

  // ex 3.17
  def dToS(ds: List[Double]): List[String] = {
    foldR(ds, Nil:List[String])((a,b) => Cons(a.toString(), b))
  }

  // ex 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldR(as, Nil:List[B])((a,b) => Cons(f(a), b))
  }

  // ex 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    def step(a:A, acc:List[A]) = {
      a match {
        case a if f(a) => Cons(a, acc)
        case _ => acc
      }
    }
    foldR(as, Nil:List[A])(step)
  }

  // ex 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldR(as, Nil:List[B])((a,b) => append(f(a), b))
  }

  // ex 3.21
  def flatFilter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a =>
      if (f(a)) List(a)
      else Nil
    )
  }

  // ex 3.22
  def addLists(as: List[Int], bs: List[Int]): List[Int] = {
    def bifoldR(as: List[Int], bs: List[Int], acc: List[Int])(f:(Int, Int, List[Int]) => List[Int]): List[Int] = {
      (as,bs) match {
        case (Nil,_) => acc
        case (_,Nil) => acc
        case (Cons(a, atail), Cons(b, btail)) => f(a, b, bifoldR(atail, btail, acc)(f))
      }
    }
    bifoldR(as, bs, Nil:List[Int])((a,b,acc) => Cons(a+b, acc))
  }

  // ex 3.23
  def zipWith[A,B,C](as: List[A])(bs: List[B])(f:(A,B)=>C): List[C] = {
    def bifoldR(as: List[A], bs: List[B], acc: List[C])(f:(A, B, List[C]) => List[C]): List[C] = {
      (as,bs) match {
        case (Nil,_) => acc
        case (_,Nil) => acc
        case (Cons(a, atail), Cons(b, btail)) => f(a, b, bifoldR(atail, btail, acc)(f))
      }
    }
    bifoldR(as, bs, Nil:List[C])((a,b,acc) => Cons(f(a,b), acc))
  }

  def main(args: Array[String]): Unit = {
    println(foldR(List(1,2,3), Nil:List[Int])(Cons(_,_)))
    println(List(1,2,3))
  }

}

