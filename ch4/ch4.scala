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
