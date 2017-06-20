/**
  * Handling errors without exceptions
  */


// not referentially transparent --> i.e. no gloab reasoning, not context dependent
def failingFn(i: Int): Int = {
  val y: Int = throw new Exception("Fail")
  try {
    val x = 42 + 5
    x + y
  }
  catch { case e: Exception => 43}
}
// because...
// diff results when we replace `y` with the exception directly
def failingFn2(i: Int): Int = {
 try {
    val x = 42 + 5
    x +  ((throw new Exception("Fail")): Int)

 }
  catch { case e: Exception => 43}
}

/* Problems with exceptions:
 *   - Introduce context dependence
 *   - Not type-safe ... failingFn Int => Int, says nothing about exceptions
 *
 * Benefit of exceptions:
 *    - Consolidate and and centralize error handling logic
 *
 * Goal:
 *    - Return error codes for exceptions
 *    - Encapsulate common patterns for handling and propagating errors
 *    - Do this ina  type-safe way, and get assistence from type checker
 */

// 4.2 - Possible alternatives to exceptions

// this is a partial functions --> not defined for some inputs
// it makes asssumptions about its inputs that aren't
// represented in the input types
def mean(xs: Seq[Double]): Double = {
  if (xs.isEmpty)
    throw new ArithmeticException("mean fo empty list!")
  else xs.sum / xs.length
}
// option 2 -- a total function
// donwnside:  requires that immediate callers have direct knowledge
// of how to handle undefined case and limits themt o returning a double
def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double = {
  if (xs.isEmpty) onEmpty
  else xs.sum / xs.length
}
/* 4.3 - The Option data type
 *      ;; defer decision on how to handle undefined cases
 *      ;; so they can be dealt with at the most appropriate level
 *
 *  Explicitly represent in the return type that the function
 *  may not always have an answer.
 *
 *  Defer to caller for error handling strategy.
 *
 *  Lets us factor out common patterns of error handling into
 *  higher order functions, so we don't have to write boilerplate.
 */
// two cases for option: 1) it is defined, wrapped in Some
//                       2) it is undefined, and a None
sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

// basically, wrap results in Some / or return None,
// and we are returning Option Type, indicating possible error
// without halting computation
def mean(xs: Seq[Double]): Option[Double] = {
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)
}

// basic functions for Option
// 4.1 - definitions
// think of Optiona as a List with only one value
trait Option[+A] {
  //def map[B](f: A => B): Option[B]
  def map[B](f: A => B): Option[B] = this match { // apply f if option is not None
      case None => None  // return nothing if a None
      case Some(a) => Some(f(a))  // otherwise, keep it wrapped and apply f(a), inside the Some
    }

  def flatMap[B](f: A => Option[B]): Option[B] = // apply f which may fail ... "
  this match {
    case None = None     // if this is None, return nothing
    case Some(a) => f(a)  // f => is already an Option, so no need to wrap in Some
  }

  def flatMap_1[B](f: A => Option[B]): Option[B] =
  // this is like .getOrElse
    map(f) getOrElse None  // so, try a map, and return a None if failure

  // the 'else' or default is what to return on failure
  // get the value (unwrapped) or nothing
  def getOrElse[B >: A](default: => Option[B]): Option[B] = // B is supertype of A
  this match {
    case None => default  // if this is None, return default
    case Some(a) => a     // otherwise, unwrap and return
  }

  // get wrapped value (or nothing)
  def orElse[B >: A](ob: => Option[B]): Option[B] =
  // again this is like .getOrElse, with a default backup, called ob to return on failure
    this map (Some(_)) getOrElse ob

  def orElse_1[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case None => ob // if this is None, return the other object
      case _ => this  // otherwise self, still wrapped
    }

  def filter(f: A => Boolean): Option[A] =  // convert some to None if value doesn't satisfy
    this match {
      case Some(a) if f(a) => this // this holds a Some(a), and if f(a) => return self
      case _ => None // otherwise none
    }

  def filter_1(f: A => Boolean): Optiona[A] =
    flatMap(a => if (f(a)) Some(a) else None) // this is an Optional fn,
                                              // and only keeps values == True
                                              // replaces others with None
                                              // so, can use a FlatMap to achieve this

}

/* Usage scenariors for basic Option functions */

// map --> transform result inside an option, if it exsits
case class Employee(name: String, department: String)
def lookupByName(name: String): Option[Employee] = ...
val joeDepartment: Option[String] =
  lookupByName("Joe").map.(_.department) // lookupByName returns an Optiona[Employee]
// and we transofrm it using map to pull out the Option[String]
// representing the departmejnt
// no need to check if it was successful

// 4.2 - Implement the variance function in terms of flatMap
// shows we can construct a computation in multiple stages
// any one of which may fail
// and abort as soon as failure is encountered
def variance(xs: Seq[Double]): Option[Double] = {
  // editorial solution
  mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
}


// Use Filter to convert sucesses into failures, if the successful values don't match predicate
// transofrm Option via cals to map, flatMap, and/or filter --> then use getOrElse to do
// error handling at te end
val dept: String =
  lookupByName("Joe").
  map(_.dept).
  filter(_ != "Accounting").
  getOrElse("Default Dept")   // converts Option[String] -> String
// orElse is similar, but return another Option, if first is undefined
// generally --> use exceptions only if No Reasonable Program could ever catch the exception


// 4.3.2 - Option Composition, lifting,
//         and wrapping exception-oriented APIs
/* Lift ordinary fucntionsto operate on option
    works by pasing whatever function we send in with our Optional.map
 */
def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

val abs0: Option[Double] => Option[Double] = lift(math.abs)
// no need to re-write functions. just lift them into the Option context, after the fact

/**
  * Top secret formula for computing an annual car
  * insurance premium from two key factors.
  */
def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double
// if user submits from web form, these values arrive as Strings, and we need to parse into integers
// but conversion can fail. so we need optional

def parseInsuranceRateQuote(
                             age: String,
                             numberOfSpeedingTickets: String): Option[Double] = {
  val optAge: Option[Int] = Try(age.toInt)
  val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
  insuranceRateQuote(optAge, optTickets) // <-- won't type check any longer
}

// function used to convert from Exception basd API to Option based API
def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None }

// Now problem is that we can't use the insuranceRateQuote fn.
// we can lift it to operate in context of Optional values


// 4.3 - write a generic function, map2 that combines two option values using a binary function
//     If either Option value is None, then the return value is too.
def map2_1[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
  (a getOrElse(None),b getOrElse(None)) match {
    case (None,_) => None
    case (_,None) => None
    case (x,y) => f(x,y)
  }

// editorial solution
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
  a flatMap(aa => b map(bb => f(aa, bb)))  // flatmap on a, because f needs to be wrapped with Option
// then we can just use map on inner function, because it has been wrapped/lifted
// flatmap does the lifting

// and with map2, now we can implement parseInsuranceRateQuote
def parseInsuranceRateQuote(
                             age: String,
                             numberOfSpeedingTickets: String): Option[Double] = {

  val optAge: Option[Int] = Try { age.toInt }
  val optTickets: Option[Int] = Try { numberOfSpeedingTickets.toInt }
  map2(optAge, optTickets)(insuranceRateQuote)
}

// 4.4 - Write a function, Sequence, that combines a list of options into one Option containing
//       a list of all the Some values in the original list. If original list contains None even once,
//       then the result of the functions should be None. Otherwise the result should be Some
//       with a list of all the values
def sequence[A](as: List[Option[A]]): Option[List[A]] =
  as match {
    case  Nil => Some(Nil) // if any Nil --> then result is Some(Nil)
    case h :: t => h flatMap( hh => sequence(t) map ( hh :: _)) // otherwise, keep recursing on sequence, using flatmap
  }

def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
  a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

// this lets us map a function over a whole list, and return None if ANY fail
// for example...
def parseInts(a: List[String]): Option[List[Int]] =
  sequence(a map (i => Try(i.toInt)))


// 4.5 -- implement traverse
def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a match {
    case Nil => Some(Nil)
    case h::t => map2(f(h), traverse(t)(f)(_ :: _)) // map(f(h)) goes in first blank, traverse(t)(f) goes in second
  }

def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
  traverse(a)(x => x)

/** For Comprehensions:
  * Lifting is essentially a series of flatmap and map calls
  * Scala has special syntax to make this easier */
// original version
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C):
Option[C] =
  a flatMap (aa =>
  b map (bb =>
  f(aa, bb)))

// And here’s the exact same code written as a for-comprehension:
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C):
Option[C] =
  for {
    aa <- a
    bb <- b
  } yield f(aa, bb)
// compiler desugas this to flatmap calls, and final call is a map

/** 4.4 - The Either data type
  *   lets us track a reason for the failure */
sealed trait Either[+E, +A]
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

def mean(xs: IndexedSeq[Double]): Either[String, Double] =
  if (xs.isEmpty)
    Left("Mean of empty list!")
  else
    Right(xs.sum / xs.length)

// factor out process of converting exceptions to values
def safeDiv(x: Int, y: Int): Either[Exception, Int] =
  try Right(x / y)
  catch { case e: Exception => Left(e)}


trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(r) => Right(f(r))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(r) = f(r)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b // b is the fallback, from Either class
    case Right(r) => Right(r)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
  Either[EE, C] =
    for {
      a <- this
      b1 <- b
    } yield f(a,b1)
  // this flatMap (tt =>
  // map (bb =>
  // f(tt, bb)))
}

// With these definitions, Either can be used in for comprehensions
def parseInsuranceRateQuote(age: String,
                           numberOfSpeedingTickets: String): Either[Exception, Double] =
  for {
    a <- Try { age.toInt }
    tickets <- Try { numberOfSpeedingTickets.toInt }
  } yield insuranceRateQuote(a, tickets) // now we get the actual exception, not just a None

// Exercise 4.7 - Implement sequence and traverse for Either
//   should return first error that's encountered, if there is one
def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
  traverse(es)(x => x)

def traverse[E, A, B](as: List[A])(
  f: A => Either[E, B]): Either[E, List[B]]
  es match {
    case  Nil => Right(Nil)
    case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
  }

def traverse_1[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
  es.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

/** Use Either to validate data */
case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

def mkName(name: String): Either[String, Name] =
  if (name == "" || name == null) Left("Name is empty.")
  else Right(new Name(name))

def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("Age is out of range.")
  else Right(new Age(age))

def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person(_, _))

