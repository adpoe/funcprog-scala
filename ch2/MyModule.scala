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
  
  def main(args: Array[String]): Unit = // Unit == void 
    println(formatAbs(-42))
}
