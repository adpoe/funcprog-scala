package funcprog.ch2
// A comment!
/* Another comment */
/** A documentation comment */
object MyModule { // a singleton object --> "defines a class and its only instance"
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc  // return acc once n <= 0
      else go(n-1, n*acc)

    go(n, 1)
  }

  // Ex 2.1
  def fibonacci(n: Int): Int = {
    // write this
    def fibHelper(x: Int, prevPrev: Int = 0, prev: Int = 1): Int = x match {
      case 0 => prevPrev
      case 1 => prev
      case _ =>  fibHelper(x-1, prev, prevPrev + prev)
    }

    fibHelper(n)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The Factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  // 2.3 - Monomorphic fn to find String in array
  def findFirstMono(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  // 2.4 - Polymorphic fn to find generic A in array
  //  - Take type A
  //  - And a function to test each type
  //       * return when fn is True
  // - Type parameter introduces type vars we can use in rest of sig
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n  // Match current element
      else loop(n + 1)

    loop(0)
  }

  // Ex 2.2 - Implement isSorted, which checks whether an Array[A] is sorted according to a
  // given comparison function:
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n+1 >= as.length) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n+1)

    loop(0)
  }

  /* Given an A, and function that needs an A and a B to produce C
     Partially apply the A.
     Now, we just need a B, and we will produce C.
   */
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)   // return a function that takes a value b of type B
                        // and applies function, using a : A as first arg

  // Ex 2.3 - Currying
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = // returns a function that takes an A
                                                    // and returns a function that takes a B
                                                    // and returns a C
    (a: A) => (b: B) => f(a, b) // an (a : A) takes a (b: B) that must return a C

  // Ex 2.4 - Uncurrying
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b) // returns a binary function, so must take 2 args
                            // and pass those args to f one at a time


  // Ex 2.5 - Implement the higher-order function that composes two functions.
  def compose[A,B,C](f: B => C, g: A => B): A => C = // need to take an A and return a C
    a => f(g(a))

  def main(args: Array[String]): Unit = // Unit == void
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(fibonacci(10))
}
