/**
  * Tree Data Structure
  *
  * algebraic data type
  * - defined by one or more data constructors
  * - each containing zero or more arguments
  * - we call it a sum or union of its data constructors
  * - and each data _constructor_ is the __product__ of its arguemnts
  */

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def size[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l,r) => 1 + size(l) + size(r)
}

// 3.26 - find maximum element of tree
def maximum_1(t: Tree[Int]): Int = {
  var m = 0
  def go(t: Tree[Int]): Unit = t match {
    case Leaf(a) => if (a > m) m = a
    case Branch(l,Leaf(_)) => go(l)
    case Branch(_,r) => go(r)
  }
  go(t)
  m
}

def maximum(t: Tree[Int]): Int = t match {
  case Leaf(n) => n
  case Branch(l,r) => maximum(l) max maximum(r) // uses method `max` fron Int
}

// 3.27 - depth
def depth(t: Tree[Int]): Int = t match {
  case Leaf(_) => 0
  case Branch(l,r) => 1 + (depth(l) max depth(r))
}

// 3.28  - map
def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
  case Leaf(n) => Leaf(f(n))  // is a Tree of B
  case Branch(l,r) => Branch(map(l)(f), map(r)(f))  // is a Tree of B, also
}


// 3.29 - generalize these similar functions on trees
def fold[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
  case Leaf(a) => f(a)
  case Branch(l,r) => Branch(fold(l)(f), fold(r)(f))
}

//fold(t)(Leaf(_))(Branch(_,_)) == t
def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = match {
  case Leaf(a) => f(a)    // f is the actual function
  case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))  // g is the constructor for branch, abstracted
                                                       // sort of the Nil, alos
}

// basically, second arg-set is what we did in Leaf, above
// author calls this a 'handler', data constructor
// on which we recursively accumulate some value
def sizeViaFold[A](t: Tree[A]): Int =
  fold(t)(a => 1)(1 + _ + _) // every elem get a 1

def maxViaFold(t: Tree[Int]): Int =
  fold(t)(a => a)( _ max _)  // everything is just itself, an take max

def depthViaFold[A](t: Tree[A]): Int =
  fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2)) // when we have nothing else,
                                              // depth is 0; it's the "Nil"

// need to wrap f(a) in a Leaf(), since we are keeping tree structure intact, not returning some other type
// this returns a Tree[B], not a B
def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
  fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))  // need to explicitly see that leaf is a tree in f's, arg