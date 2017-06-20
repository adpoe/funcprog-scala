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
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty // create empty stream of particular type

  def apply[A](as: A*): Stream[A] = // construct a stream w/ multiple elems
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}

// optionally extract the head of a Stream
def headOption: Option[A] = this match {
  case None => Empty
  case Cons(h, t) => Some(h()) // explicitly force h thunk, using h()
}

// so, we have to force thunks here. but otherwise, the code works just
// as we would expect for a list.
// however, the ability of a stream only evaluate when actually demanded
// is useful. think: python generators...
