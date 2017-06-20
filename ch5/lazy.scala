/** Strict and Lazy
  * Function Evaluation
  * in Scala
  */

// lazy if-statement
def if2[A](cond: Boolean, onTrue: () => A,
           onFalse: () => A): A =
  if (cond) onTrue() else onFalse()

// un-evaluated part of expression is called a thunk
// () is a fn that takes no args and returns an A
def if2[A](cond: Boolean, onTrue: => A,
                          onFalse: => A): A =
  if (cond) onTrue() else onFalse()

// i.e. - if2(false, sys.error("fail"),3)
// => Int = 3


/* 5.2 - Simple definition for Stream */

sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

// This is just like our list type, but it takes explicit thunks
// (() => A and () => Stream[A]), instead of regular strict values
// in order to examine or traverse the stream, we need to force the thunks
// like in if2
object Stream {
  // use this smart constructors for memoization,
  // so that values are only evaluated one
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }
  // smart constructor
  def empty[A]: Stream[A] = Empty // create empty stream of particular type

  def apply[A](as: A*): Stream[A] = // construct a stream w/ multiple elems
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    // optionally extract the head of a Stream
  def headOption: Option[A] = this match {
      case None => Empty
      case Cons(h, t) => Some(h()) // explicitly force h thunk, using h()
    }
  // so, we have to force thunks here. but otherwise, the code works just
  // as we would expect for a list.
  // however, the ability of a stream only evaluate when actually demanded
  // is useful. think: python generators...

  // Ex 5.1 - convert a stream to a list
  def tolist: List[a] = this match {
    case Cons(h, t) => h :: t().toList // we have to invoke t() as a method
    // it is a thunk
    case _ => List() // otherwise, it's an empty list
  }

  // Ex 5.2 -  take(n) ; and drop(n)
  def take(n: Int): Stream[A] = this match { // need to evaluate each thunk
    case Cons(h,t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h,_) if n == 1 => cons(h(), empty)
    case _ => empty // empty smart constructor
  }

  @annotation.tailrec
  def drop(n: Int): Stream[A] = this match {
    case Cons(_,t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

 // Ex 5.3 - takeWhile( predicate == True)
  def takeWhile(p: A => Boolean): Stream[A] = this match {
   case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
   case  _ => empty
 }

}


