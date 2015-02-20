package example.misc

import scala.annotation.tailrec

object Fact extends App {

  def fact1(n: Long): Long = {
    n match {
      case 0 => 1
      case _ => n * fact1(n - 1)
    }
  }

  def fact2(n: Long): Long = {
    @tailrec
    def fact0(result: Long, n: Long): Long =
      n match {
        case 0 => result
        case _ => fact0(n * result, n - 1)
      }
    fact0(1, n)
  }

  println("fact1 = " + fact1(10))
  println("fact2 = " + fact2(10))

}
