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
  def concat[A](ls: List[List[A]], n:List[A]=Nil): List[A] =
    foldRight(ls, n)((h : List[A], t: List[A]) => append(h,t))

    /* More list functions:
     *   - Generalize any explicit recursive functions written while
     *     processing ists
     *   - and will rediscover these funcions, developing an instinct
     *     for when to use
     */

  // transform list of ints by adding 1 to each element
  def addOne(l: List[Int]): List[Int] = // walk through with Cons and
                                        // just add one to each head
                                        // as we pass, and then Cons
                                        // and keep going
    foldRight(l, Nil: List[Int])((h,t) => Cons(h+1, t))
  // Smart way to do it.


  // 3.17 - write fn that turns each value in a List[Double] to String.
  //   use d.toString to perform conversion
  def doubleList2String(l: List[Double]): List[String] =
    foldRight(l, Nil: List[Int])((h,t) => Cons(h.toString, t))

  // 3.18 - Generalize these functions, with map
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[Int])((h,t) => Cons(f(h), t)) // simple but not stack safe


  def map_2[A,B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    // use an inner function that only sees one item at a time
    // to keep it stack safe
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t) // add current head to buffer,
                                           // outside inner fn scope
                                           // and keep recursing
    }
    go(l)
    List(buf.toList: _*)// convert to our list type from ListBuffer
  }

  // 3.19 - Write a function filter that removes elements from a list unless they satisfy a given
  // predicate. Use it to remove all odd numbers from a List[Int].
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h,t) if f(h) => Cons(h, filter(t)(f))
    case Cons(_,t) => filter(t)(f)
  }
  // keep only evens
  // filter(as)((x: Int) => x % 2 == 0)

  // or, with a foldRight
  def filterFold[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t) // nice one liner

  // and with stack safety
  def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A] // mutable value
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => if (f(h)) buf += h; go(t) // add to external buf & recurse
    }
    go(l)
    List(buf.toList: _*)
  }

  // 3.20 - FlatMap
  // same as map, but use concat to combine, since we are combining lists now
  def flatMap_1[A,B](as: List[A])(f: A => List[B]): List[B] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => concat(buf, f(h)); go(t) // use concat because
                                                 // it combines 2 lists

    }
    go(l)
    List(buf.toList: _*)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // 3.21 - Use flatMap to implement filter
  def filterFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil) // basically need to convert
                                                // each item to a list
                                                // so that flatMap works

  // 3.22 - Add lists pairwise
  def addLists[A](l: List[A], r: List[A]): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A], r: List[B]): Unit = (l, r) match {
      case (Nil, _) => ()
      case (_, Nil) => ()
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, go(xs, ys))
    }
    go(l)
    List(buf.toList: _*)
  }

  // 3.23 - ZipWith
  // stack-safe
  def zipWith[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] => {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A], r: List[B]): Unit = (l, r) match {
      case (Nil, _) => ()
      case (_, Nil) => ()
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), go(xs, ys))
  }
    go(l)
    List(buf.toList: _*)
  }

  // simpler
  def zipWith_1[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] =
    (a,b) match {
      case (Nil,_) => Nil
      case (_,Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith_1(xs, ys))
    }

  // 3.24 -  Subsequences -- monolithic approach
  //   * better to use combinatoins of other small functions
  //   * makes code easier to reason about
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean =
    (l,prefix) match {
      case (_,Nil) => true // if prefix is Nil --> base case. happens when run out of prefix
      case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t,t2) // check item by item
      // and keep recursing until we run out of t2
      case _ => false // any other case is false
    }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
  sup match {
    case Nil => sub == Nil  // if we hit a Nil in the target string
                            // then also need to check if the sub is Nil at same point
                            // if so --> they are ==
    case _ if startsWith(sup, sub) => true  // any other case,
                                            // first, play out startsWith to end
                                            // and if it returns true --> we are true
    case Cons(_,t) => hasSubsequence(t, sub)  // otherwise, drop head of list
                                              // and keep going
  } // this makes it sort of like a for-loop, calling fn's at each step
    // interesting approach



}
