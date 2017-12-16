object Optics {

  trait Iso[S,A] {
    def get(s:S): A
    def reverseGet(a:A): S
  }
  case class Prism[S,A](getOption: S=>Option[A], reverseGet: A=>S) {

    def isMatching(s:S): Boolean
    def modify(f: A=>A): S=>S // if this modify "fails", it returns the original S
    def modifyOption(f: A=>A): S=>Option[S]
    def compose[B](other: Prism[A,B]): Prism[S,B]


    /**
      * Iso - Prism
      *
      * Optic   f             g
      * -----------------------
      * Iso     S=>A          A=>S
      * Prism   S=>Option[A]  A=>S
      * Lens    S=>A          (A,S)=>S
      *
      * @param other
      * @tparam B
      * @return
      */
    def compose[B](other: Iso[A,B]): Prism[S,B]

  }
}

