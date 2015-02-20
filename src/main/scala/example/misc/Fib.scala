package example.misc

object Fib extends App {

  def fib1(n: Long): Long = if(n < 2) n else fib1(n - 2) + fib1(n - 1)

  def fib2(n: Long): Long = {
    n match {
      case 0 | 1 => n
      case v if v >= 2 =>
        fib2(v - 2) + fib2(v - 1)
    }
  }

  println("fib1 = " + fib1(10))
  println("fib2 = " + fib2(10))

}
