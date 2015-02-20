package example.misc

object WordCount extends App {

  def wordcount1(lines: Seq[String]) = {
    // まずList[String]にしたい
    // lines.map(e => e.split(" ").toList) とすると
    // List(List("a","b"), List("c")) のような List[List[String]] になってしまう。
    // その側のListは不要。そういう時はflatMapを使う。
    val fruits = lines.flatMap(_.split(" ").toSeq)

    // それぞれの項目が何個あるかチェックする
    val result = fruits.toSet[String].map(e => (e, fruits.count(_ == e))).toMap
    assert(result == Map("banana" -> 1, "muscat" -> 1, "orange" -> 3, "mango" -> 2, "apple" -> 3, "kiwi" -> 1, "papaya" -> 1))
    result
  }

  def wordcount2(lines: Seq[String]) = {
    val fruits = lines.flatMap(_.split(" ").toSeq)
    val result = fruits.groupBy(identity).map{e => (e._1, e._2.size)}
    assert(result == Map("banana" -> 1, "muscat" -> 1, "orange" -> 3, "mango" -> 2, "apple" -> 3, "kiwi" -> 1, "papaya" -> 1))
    result
  }

  val lines = Seq("apple banana", "orange apple mango", "kiwi papaya orange", "mango orange muscat apple")

  println(wordcount1(lines))
  println(wordcount2(lines))

}
