package funcprog.chapter2
// A comment!
/* Another comment */
/** A documentation comment */
object MyModule { // a singleton object --> "defines a class and its only instance"
  def abs(n: Int): Int =
    if (n < 0) -n
    else n
    
 // private method can only be called by other methods of MyModule
  private def formatAbs(x: Int) = {
    val msg = "the absolute value of %d is %d"
    msg.format(x, abs(x))
 }

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

  def main(args: Array[String]): Unit = // Unit == void 
    println(formatAbs(-42))
    println(factorial(7))
    println(fibonacci(10))
}
