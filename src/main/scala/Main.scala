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

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  // 単相関数
  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = 
      if (n >= ss.length) -1
      else if (ss(n) == key) n // 配列ssのn番目
      else loop(n+1)

    loop(0)
  }

  // 多相関数
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = 
      if (n >= as.length) -1
      else if (p(as(n))) n // pの引数に配列asのn番目
      else loop(n+1)

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = 
      if (n>=as.length-1) true
      else if (ordered(as(n), as(n+1))) false
      else loop(n+1)

    loop(0)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) =
    a => b => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 5, factorial))
    println(formatResult("fib", 7, fib))
    println(isSorted(Array(1, 2, 3), (x: Int, y:Int) => x > y))
    println(isSorted(Array(5, 2, 3), (x: Int, y:Int) => x > y))
  }
}