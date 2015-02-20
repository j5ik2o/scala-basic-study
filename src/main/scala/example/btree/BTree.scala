package example.btree

import scala.collection.immutable.NumericRange

sealed trait Node[+A] {

  val value: A

  val size: Int
  val sum: A
  val avg: A
  val max: A
  val min: A

  def find[B >: A](value: B): Option[Node[B]]

  def map[B >: A](f: A => B)(implicit O: Ordering[B], F: Fractional[B]): Node[B]

}

case class Leaf[A](value: A) extends Node[A] {

  val size = 1
  val sum = value
  val avg = value
  val max = value
  val min = value

  def find[B >: A](value: B): Option[Node[B]] =
    if (this.value == value) Some(this) else None

  def map[B >: A](f: (A) => B)(implicit O: Ordering[B], F: Fractional[B]): Node[B] = Leaf(f(value))

}

case class Branch[A](left: Node[A], value: A, right: Node[A])
                    (implicit O: Ordering[A], F: Fractional[A]) extends Node[A] {

  require(O.lt(left.value, value))
  require(O.lt(value, right.value))

  lazy val max: A = right.max

  lazy val min: A = left.min

  lazy val size: Int = left.size + 1 + right.size

  lazy val sum: A = List(left.sum, value, right.sum).sum

  lazy val avg: A = F.div(sum, F.fromInt(size))

  def find[B >: A](value: B): Option[Node[B]] = this match {
    case Branch(_, v, _) if v == value => Some(this)
    case Branch(l, v, _) if O.lt(value.asInstanceOf[A], v) => l.find(value)
    case Branch(_, v, r) if O.lt(v, value.asInstanceOf[A]) => r.find(value)
  }

  def map[B >: A](f: (A) => B)(implicit O: Ordering[B], F: Fractional[B]): Node[B] =
    Branch(left.map(f), f(value), right.map(f))

}

case class BTree[A](node: Node[A])(implicit F: Fractional[A]) {

  lazy val size: Int = node.size

  lazy val max: A = node.max

  lazy val min: A = node.min

  lazy val sum: A = node.sum

  lazy val avg: A = F.div(sum, F.fromInt(size))

  def find(number: A): Option[Node[A]] = node.find(number)

  def map[B >: A](f: (A) => B)(implicit O: Ordering[B], F: Fractional[B]): Node[B] = node.map(f)

}

object BTree {

  def from[A](values: Seq[A])(implicit O: Ordering[A], F: Fractional[A]): Node[A] = {
    if (values.size == 1) {
      Leaf(values.head)
    } else {
      require(values.size % 2 == 1)
      val (left, mid :: right) = values.splitAt(values.size / 2)
      Branch(from(left), mid, from(right))
    }
  }

}

object Main extends App {

  implicit val integral = scala.math.Numeric.BigDecimalAsIfIntegral

  val numbers = NumericRange(BigDecimal(1), BigDecimal(8000000), BigDecimal(1)).toList

  val node = BTree.from(numbers)

  println("max = " + node.max)
  println("min = " + node.min)
  println("sum = " + node.sum)
  println("avg = " + node.avg)

  println("find(3) = " + node.find(BigDecimal(3)))
  println(node.map(_ * 2))

}
