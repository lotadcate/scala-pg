object MyModule {
  def abs(n: Int): Int =
    if (n<0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n<=0) acc
      else go(n-1, n*acc)
    }
    go(n,1)
  }

  def fib(n: Int): Int = {
    if (n==1||n==2) 1
    else fib(n-1) + fib(n-2)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  private def formatFib(n: Int) = {
    val msg = "The fib of %d is %d"
    msg.format(n, fib(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(5))
    println(formatFib(7))
  }
}