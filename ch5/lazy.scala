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


/* 5.3 - Separating Program Description from Evaluation */
def exists(p: A => Boolean): Boolean = this match {
  case Cons(h,t) => p(h()) || t.exists(p)
  case _ => false
}

// lazy foldright
def foldRight[B](z: => B)(f: (A, => B) => B): B =
  this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

// exists via foldright
def exists(p: A => Boolean): Boolean =
  foldRight(false)((a,b) => p(a) || b) // b is unevaluated recursive step that
                                       // folds the tail of the stream
                                       // if f chooses not to evaluate its
                                       // 2nd param, traversal terminates early

// Ex 5.4 - forAll ... terminate as soon as encounter non-matching value
def forAll(p: A => Boolean): Boolean =
  foldRight(true)((a,b) => p(a) && b) // terminate as soon as a p(a) is false

// Ex 5.5 -takeWhile via foldirght
def takeWhile(f: A => Boolean): Stream[A] =
  foldRight(empty[A])((h,t) =>
    if (f(h)) cons(h,t)
    else      empty)

// Ex 5.6 -  Implement headOption using foldright
def headOption: Option[A] =
  foldRight(None: Option[A])((h,_) => Some(h))

// Ex 5.7 - map, filter, append, and flatMap using foldRight
def map[B](f: A => B): Stream[B] =
  foldRight(empty[B])((h,t) => cons(f(h), t))

def filter(f: A => Boolean): Stream[A] =
  foldRight(empty[A])((h,t) =>
    if (f(h)) cons(h,t)
    else t)

def append[B>:A](s: => Stream[B]): Stream[B] =
  foldRight(s)((h,t) => cons(h,t))

def flatMap[B](f: A => Stream[B]): Stream[B] =
  foldRight(empty[B])((h,t) => f(h) append t)


/**
  * 5.4 - Infinite streams and corecursion
  */
val ones: Stream[Int] = Stream.cons(1, ones)
// 5.8 - generalize ones to the fn 'constant' which returns
//   an infinite recursive stream of a give value
def constant[A](a: A): Stream[A] =
  Stream.cons(a, constant(a))

// more efficient version
// bc it's just one object referencing itself
def constant[A](a: A): Stream[A] = {
  lazy val tail: Stream[A] = Cons(() => a, () => tail)
  tail
}

// 5.9 - fn to gen an infnite stream of ints
//    n + 1, n + 2
def from(n: Int): Stream[Int] =
  Stream.cons(n, from(n+1))

// 5.10 - write a fn, fibs, that generates an infinite
// stream of fib numbers, 0,1,1,2,3,5,8, etc..
val fibs = {
  def go(f0: Int, f1: Int): Stream[Int] =
    cons(f0, go(f1, f0+f1)) // next value is f0 + f1
  go(0,1)
}

// 5.11 - write a more general stream-building fn
// called unfold. takes an initial sate, and a fn
// for producing both the next state, and the next value
// in the generated stream
def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] =
  f(z) match { // run a fucntion, and if evals to a Some, keep going
    case Some((h,s)) => cons(h, unfold(s)(f))
    case None => empty
  } // a corecusrve function. produces data.
    // fn is productive, as long as f terminates
    // always only need to run fn *one* more time
    // to get next result in the stream
    // -- also called guarded recursion

// 5.12 - write fibs, constant, and ones, in terms of unfold
val fibsViaUnfold =
  unfold((0,1)) { case (f0, f1) => Some((f0, (f1,f0+f1)))}

def fromViaUnfold(n: Int) =
  unfold(n)(n => Some((n,n+1)))

def constantViaUnfold[A](a: A) =
  unfold(a)(_ => Some((a,a))) // always same

def onesViaUnfold = unfold(1)(_ => Some((1,1)))


