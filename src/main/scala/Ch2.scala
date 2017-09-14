package fpclub

object Ch2 {
	def abs(x: Int): Int =
		if (x > 0) x
		else -x

	private def formatResult(s: String, x: Int, f: Int => Int) =
		"The %s of %d is %d".format(s, x, f(x))

	def fact(n: Int): Int = {
		@annotation.tailrec
		def _fact(n: Int, acc: Int): Int =
			if (n <= 0) acc
			else _fact(n-1, n*acc)

		_fact(n, 1)
	}

	def fib(n: Int): Int = {
		if (n<0) throw new NumberFormatException("input must be non-negative: %d".format(n))
		@annotation.tailrec
		def _fib(n: Int, a: Int, b: Int): Int = {
			n match {
				case 0 => a
				case _ => _fib(n-1, b, a+b)
			}
		}
		_fib(n, 0, 1)
	}


	def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
		if (as.length == 0) return true
		@annotation.tailrec
		def _isSorted(n: Int, prev: A): Boolean = {
			if (n >= as.length)
				true
			else if (!ordered(prev, as(n)))
				false
			else
				_isSorted(n+1, as(n))
		}
		_isSorted(0, as(0))
	}

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a, b)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a:A, b:B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

	def main(args: Array[String]): Unit = {
		println(formatResult("absolute value", -42, abs))
		println(formatResult("factorial", 6, fact))
		for (x <- 0 to 6)
			println("fib %d: %d".format(x, fib(x)))
	}
}

