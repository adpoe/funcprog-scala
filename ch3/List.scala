package funcprog

sealed trait List[+A] // List data type paramaterized on type A
case object Nil extends List[Nothing] // A list constructor represents empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Non empty list
                             // Tail is another list
                             // which may be Nil or another Cons

/** List companion object.
  *
  * Contains functions for creating and working with lists
  */
object List {
  def sum(ints: List[Int]): Int = ints match { // fn uses pattern matching
    case Nil => 0  // Sum of empty list is 0
    case Cons(x, xs) => x + sum(xs) // sum of list starting at x = x + rest
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // variadic fn syntax
    if (as.isEmpty) Nil // The special _* type annotation
                        // allows us to pass a Seq to
                        // a variadic method.
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l : List[A]): List[A] = l match {
    case Nil => Nil  // can also throw an error here
    case Cons(x, xs) => xs
  }

  def setHead[A](l : List[A], h : A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  /* cool part about this recursive function:
   *   - we catch the if every time, within the pattern matching recursive call
   *   - so we get the base case at the start (if case)
   *   - and it's embedded within pattern matching
   */
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l // if nothing to drop, just return the list
    else l match { // otherwise, match on the list
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1) // can directly use fn to recurse
                                     // discarding first elem
                                     // each time
    }

  def dropWhileV1[A](l: List[A], f: A => Boolean): List[A] =
    if (!f(l(0))) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => dropWhile(t,f)
    }

  /*
  Somewhat overkill, but to illustrate the feature we're using a _pattern guard_,
  to only match a `Cons` whose head satisfies our predicate, `f`.
  The syntax is to add `if <cond>` after the pattern, before the `=>`,
  where `<cond>` can use any of the variables introduced by the pattern.
  */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f) // add an if in a case clause... and recurse
      case _ => l  // base case is that f is no long true
    }

  // add all elements of one list the end of another
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match { //
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2)) // works because both t and a2 are lists
        // traverse to end of a1, and thne append a2, as a whole
        // List built:  Cons(x1, Cons(x2, Cons(x3, Nil => a2)))
    }

  // simple way
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

  // use internal buffer to prevent stack overflow
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): Lista[A] = cur match {
      case Nil => Nil
      case Cons(_,Nil) => List(buf.toList: _*)
      case cons(h,t) => buf += h; go(t)
    }
    go(l)
  }

  // Improving type inferece for higher-order functions
  //   - works if we group the two argument lists
  //   - then don't need to specify the type in anonymous functions for group2
  //   - basically, we curry the function,
  //      * so that it knows the type of A when the second arg is passed in
  //      * group and order fn args into multiple lists to maximze type inference
  //      * type inference flows left --> right across argument groups
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h,t) if f(h) => dropWhile(t)(f)
      case _ => as
    }

  /* Recursion over lists and generalizing higher order fn's */
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product1(ds: List[Double]): Double = ds match {
    case Nil => 1.0  // end of list --> * 1.0
    case Cons(x,xs) => x * product1(xs)
  }
  // only differences here are the types and values to return in case of a Nil
  // and operation to combine results
  // whenever there's duplication like this, you can generalize it away by
  // pulling subexpressions into function arguments
  // if subexpression refers to any local variables (i.e. + and * ops w/ x and xs
  // then turn subespression into a function that accepts these variable args
  // for example, we'll do that next.
  // the function should take as args the value to return in case of empty list
  // and the function to add an element in the case of a nonempty list

  /* f goes in its own argument group after `as` and `z`
   * and that lets type inference determine the input types to f
   */
  def foldRight[A,B](as: List[A], z:B)(f: (A,B) => B): B =
    as match {
      case Nil => z  // z holds the value to use if empty, accumulator
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))  // apply fn, and keep recursing
    } // gets us:  Plus(1, Plus(2, Plus(3, foldRight(xs, z(f)))))
      //           Plus(1, Plus(2, Plus(3, 0))) ; where z=0

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)  // this is a lambda, used to combine
            // ns is the list, 0 is the accumulator

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // and so is this
                              // _ * _ is (x,y) => x * y

  /* One way of describing what foldRight does is that it replaces the constructors
     of the list, Nil and Cons, with z and f, illustrated here:
        Cons(1, Cons(2, Nil))
        f (1, f (2, z )) */

  // compute list length, using foldRight
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_,acc) => acc + 1) // add 1 for every elem of list

  /* foldRight is not stack safe. Write a tail recursive foldLeft that is.
   * - Here stack evaluation starts at LEFT
   * - Which means we don't have to traverse all the way to end
   * - and build whole stack before evluation happens
   * - storing full list in memory
   */
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
  // think: build list starting at RIGHTMOST value, starts at right side
    as match {
      case Nil => z                  // accum
      case Cons(x,xs) => foldLeft(xs, f(z, x))(f) // call self directly
                                                  // and wrap f(z,x) as accum
                                                  // which means fn eval
                                                  // happens immediately
                 // vs:  f(x, foldRight(xs, z)(f))
    }

  /* write sum, product, and fn to compute length, using foldleft */
  def sumL(as: List[Int]): Int = foldLeft(as, 0)(_ + _)  // order doesn't matter

  def product(l: List[Doube]): Double = foldLeft(l, 0.0)(_ * _) // again commutative

  def lengthL[A](l: List[A]): Int = foldLeft(l,0)((acc,_) => acc + 1) // acc is first now

  /* reverse a list using a fold */
  def reverse[A](l: List[A]): List[A] = foldRight(l, Nil)((x, xs) => Cons(xs, x))
  // didn't need to match, since we are doing it in the lambda

  // and a left folding version
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h, acc))

  /** write fold left in terms of fold right */
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  // append with a fold
  def appendFold[A](l: List[A], r: List[A]) List[A] = foldRight(l, r)(Cons(_,_))

  // fn that concats a list of lists into a single list. O(n) time.
  def concatLists(ls: List[List[A]], n:List[A]=Nil): List[A] =
    foldRight(ls, n)((h : List[A], t: List[A]) => append(h,t))


}
