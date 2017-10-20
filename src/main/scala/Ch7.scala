package fpclub7

import java.util.concurrent

import fpclub7.Ch7.{Callable, ExecutorService}

import scala.concurrent.duration.TimeUnit

object Ch7 {

  type Par[A] = ExecutorService => Future[A]

  object Par {
    /**
      * Creates a computation that immediately results in the value `a`
      *
      * @param a
      * @tparam A
      * @return
      */
    def unit[A](a: A): Par[A] = _ => UnitFuture(a)

    /**
      * Marks a computation for concurrent evaluation by `run`
      *
      * @param a
      * @tparam A
      * @return
      */
    def fork[A](a: => Par[A]): Par[A] = exec => {
      exec.submit(new Callable[A] {
        def call = a(exec).get
      })
    }

    /**
      * Wraps expression `a` for concurrent evaluation by `run`
      *
      * @param a
      * @tparam A
      * @return
      */
    def lazyUnit[A](a: => A) = fork(unit(a))

    // ex 7.1
    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B)=>C): Par[C] = exec => {
      val ra = a(exec).get
      val rb = b(exec).get
      UnitFuture(f(ra,rb))
    }

    /**
      * Runs the given `Par`, spawning parallel computations as
      * marked, and returns the `Future` result.
      *
      * @param a
      * @tparam A
      * @return
      */
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = {
      a(s)
    }

    // ex 7.4
    def asyncF[A,B](f: A=>B): A => Par[B] = {
      a => lazyUnit(f(a))
    }

    def map[A,B](pa: Par[A])(f: A=>B): Par[B] = {
      lift(f)(pa)
    }

    def lift[A,B](f: A=>B): Par[A] => Par[B] = {
      pa => map2(pa, unit(()))((a,_) => f(a))
    }

    /**
      * Map a function `f` over a list in _parallel_
      *
      * @param ps
      * @param f
      * @tparam A
      * @tparam B
      * @return
      */
    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork({
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    })

    // ex 7.5
    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      def init:Par[List[A]] = unit(Nil:List[A])
      ps.foldRight(init)((pa, parList) => {
        map2(pa, parList)((a,as) => a::as)
      })
    }

    // ex 7.6
    def parFilter[A](as: List[A])(f: A=>Boolean): Par[List[A]] = {
      val af: A => Par[Option[A]] = asyncF( a => if (f(a)) Some(a) else None )
      val ps: List[Par[Option[A]]] = as.map(af)
      ps.foldRight(unit(Nil:List[A]))((pa, parList) => {
        map2(pa, parList)((a, as) => a match {
          case Some(_a) => _a::as
          case _ => as
        })
      })
    }

    /**
      * Counts the number of words in the given paragraphs
      * in parallel
      *
      * @param paragraphs
      * @return
      */
    def parCountWords(paragraphs: List[String]): Par[Int] = {
      val wordsInStr = (s : String) => s.split("\\s+").size
      val pars: Par[List[Int]] = parMap(paragraphs)(wordsInStr)
      Par.map(pars)(counts => counts.foldLeft(0)(_+_))
    }

    // ex 7.7
    /**
      * Given `map(y)(id) == y`, it is a free theorem that `map(map(y)(g))(f) == map(y)(f.g)`
      *
      * Lemma:
      * ```
      * given: map(y)(id) = y
      *
      * let: unit(x) = y
      *
      * >> map(unit(x))(id) = unit(x)
      *
      * let: f = id
      *
      * Lemma1 => map(unit(x))(f) = unit(f(x))
      * ```
      *
      * Proof:
      * ```
      * >> map(map(y)(g))(f)
      *
      * let: unit(x) = y
      *
      * >> map(map(y)(g))(f) = map( map(unit(x))(g) )(f)                [1]
      *                                    |
      * apply Lemma1 to: map(unit(x))(g)   '----.
      *                                         v
      * >> map( map(unit(x))(g) )(f) = map( unit(g(x)) )(f)             [2]
      *
      * apply Lemma1 to: map(unit(g(x)))(f)
      *
      * >> map( unit(g(x)) )(f) = unit( f(g(x)) ) = unit( f.g(x) )      [3]
      *
      * let: h = f.g
      *
      * >> unit(h(x)) = map(unit(x))(h) = map(unit(x))(f.g)             [4]
      *
      * from [1]:
      *
      * >> map(map(unit(x))(g))(f) = map(unit(x))(f.g)
      *
      * QED => map(map(y))(g)(f) = map(y)(f.g)
      * ```
      */


    // ex 7.11
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
      choices.drop(run(es)(n).get).head(es)
    }
    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
      choiceN(Par.map(cond)(if (_) 0 else 1))(
        List(t,f)
      )
    }

    // ex 7.13
    def chooser[A,B](p: Par[A])(choices: A=>Par[B]): Par[B] = es => {
      choices(run(es)(p))(es)
    }
    def choiceN_asChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
      chooser(n)(choices.drop(_).head)
    }
    def choice_asChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
      chooser(cond)(if (_) t else f)
    }
    // `chooser` is really too specific a name as it is now
    // more general than the act of making a choice.  we
    // call it `flatmap` (or `bind`)
    def flatMap[A,B](p: Par[A])(f: A=>Par[B]): Par[B] = es => {
      f(run(es)(p))(es)
    }

    // ex 7.14
    /**
      * Returns a Par that waits for an inner parallel computation to complete
      * before returning the result
      *
      * @param a
      * @tparam A
      * @return
      */
    def join[A](a: Par[Par[A]]): Par[A] = es => {
      map(a)(run(es)(_).get)(es)
    }



  }

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)
  class ParOps[A](p: Par[A]) {
    def map2[B,C](b: Par[B])(f: (A,B)=>C) = Par.map2(p,b)(f)
  }



  class ExecutorService {
    val jEx: java.util.concurrent.ExecutorService = java.util.concurrent.Executors.newFixedThreadPool(10)
    def submit[A](a: Callable[A]): Future[A] = JavaFuture(jEx.submit(() => a.call))
  }
  trait Callable[A] {
    def call: A
  }
  trait Future[A] {
    def get:A
    def get(timeout:Long, unit:TimeUnit): A
    def cancel(evenIfRunning:Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean
  }

  private case class JavaFuture[A](jF: concurrent.Future[A]) extends Future[A] {
    override def get: A = jF.get
    override def get(timeout: Long, unit: TimeUnit): A = jF.get(timeout, unit)
    override def cancel(evenIfRunning: Boolean): Boolean = jF.cancel(evenIfRunning)
    override def isDone: Boolean = jF.isDone
    override def isCancelled: Boolean = jF.isCancelled
  }
  private case class UnitFuture[A](get:A) extends Future[A] {
    override def get(timeout:Long, unit:TimeUnit): A = get
    override def cancel(evenIfRunning:Boolean): Boolean = false
    override def isDone: Boolean = true
    override def isCancelled: Boolean = false
  }
}

//object Actors {
//  sealed trait Future[A] {
//    private[fpclub7] def apply(k: A => Unit): Unit
//  }
//  type Par[+A] = ExecutorService => Future[A]
//
//  object Par {
//    def unit[A](a: A): Par[A] = _ => new Future[A] {
//      def apply(k: A => Unit): Unit = k(a)
//    }
//
//    def fork[A](a: => Par[A]): Par[A] = exec => {
//      // evaluate an action asynchronously using an ExecutorService
//      def eval(e: ExecutorService)(r: => Unit): Unit = {
//        e.submit(new Callable[Unit] { def call = r })
//      }
//      new Future[A] {
//        override private[fpclub7] def apply(k: (A) => Unit): Unit = {
//          eval(exec)(a(exec)(k))
//        }
//      }
//    }
//
//    def lazyUnit[A](a: => A) = fork(unit(a))
//
//    // ex 7.1
//    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = exec => {
//      new Future[C] {
//        override private[fpclub7] def apply(k: (C) => Unit): Unit = {
//          var ar: Option[A] = None
//          var br: Option[B] = None
//          val combiner = Actor[Either[A,B]](exec) {
//
//          }
//        }
//      }
//    }
//
//    /**
//      * Runs the given `Par`, spawning parallel computations as
//      * marked, and returns the `Future` result.
//      *
//      * @param a
//      * @tparam A
//      * @return
//      */
//    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = {
//      a(s)
//    }
//
//    // ex 7.4
//    def asyncF[A, B](f: A => B): A => Par[B] = {
//      a => lazyUnit(f(a))
//    }
//
//    def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
//      lift(f)(pa)
//    }
//
//    def lift[A, B](f: A => B): Par[A] => Par[B] = {
//      pa => map2(pa, unit(()))((a, _) => f(a))
//    }
//
//    /**
//      * Map a function `f` over a list in _parallel_
//      *
//      * @param ps
//      * @param f
//      * @tparam A
//      * @tparam B
//      * @return
//      */
//    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork({
//      val fbs: List[Par[B]] = ps.map(asyncF(f))
//      sequence(fbs)
//    })
//
//    // ex 7.5
//    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
//      def init: Par[List[A]] = unit(Nil: List[A])
//
//      ps.foldRight(init)((pa, parList) => {
//        map2(pa, parList)((a, as) => a :: as)
//      })
//    }
//
//    // ex 7.6
//    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
//      val af: A => Par[Option[A]] = asyncF(a => if (f(a)) Some(a) else None)
//      val ps: List[Par[Option[A]]] = as.map(af)
//      ps.foldRight(unit(Nil: List[A]))((pa, parList) => {
//        map2(pa, parList)((a, as) => a match {
//          case Some(_a) => _a :: as
//          case _ => as
//        })
//      })
//    }
//  }
//}
