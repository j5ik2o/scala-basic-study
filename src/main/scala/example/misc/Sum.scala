package example.misc

import scala.annotation.tailrec

object Sum extends App {

  def getSum1(numbers: Seq[Long]): Long = {
    numbers.toList match {
      case Nil => 0
      case x :: xs => x + getSum1(xs)
    }
  }


  def getSum2(numbers: Seq[Long]): Long = {
    @tailrec
    def getSum0(result: Long, tailNumbers: List[Long]): Long = {
      tailNumbers match {
        case Nil => result
        case x :: xs => getSum0(result + x, xs)
      }
    }
    getSum0(0, numbers.toList)
  }

  def getSum3(numbers: Seq[Long]): Long =
    numbers.reduceLeft(_ + _)

  def getSum4(numbers: Seq[Long]): Long =
    numbers.sum


  println("getSum1 = " + getSum1(1L to 100L))
  try {
    println("getSum1 = " + getSum1(1L to 100000L))
  } catch {
    case ex: Throwable => println(ex)
  }
  println("getSum2 = " + getSum2(1L to 100L))
  println("getSum2 = " + getSum2(1L to 100000L))
  println("getSum3 = " + getSum3(1L to 100L))
  println("getSum3 = " + getSum3(1L to 100000L))
  println("getSum4 = " + getSum4(1L to 100L))
  println("getSum4 = " + getSum4(1L to 100000L))

}
