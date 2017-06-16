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

  def tail(l : List[A]): List[A] = l match {
    case Nil => Nil  // can also throw an error here
    case Cons(x, xs) => xs
  }

  def setHead(l : List[A], h : A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }


}