object S99Logic {
  implicit def toMyBool(a: Boolean) = new MyBool(a)

  def and(a: Boolean, b: Boolean) = a && b
  def or(a: Boolean, b: Boolean) = a || b
  def xor(a: Boolean, b: Boolean) = a ^ b

  def table2(f: (Boolean, Boolean) => Boolean) {
    val format = "%-5s %-5s %-6s\n";
    printf(format, "A", "B", "result")
    val bools = List(true, false)
    for (a <- bools; b <- bools) printf(format, a, b, f(a, b))
  }

  def gray(n: Int): List[String] = n match {
    case 0 => List("")
    case _ =>
      val lower = gray(n - 1)
      lower.map("0" + _) ::: lower.reverse.map("1" + _)
  }

  val grayMemoizedMap = scala.collection.mutable.Map(0 -> List(""))
  def grayMemoized(n: Int): List[String] = {
    if (grayMemoizedMap.contains(n)) grayMemoizedMap(n)
    else {
      val lower = grayMemoized(n - 1)
      val result = lower.map("0" + _) ::: lower.reverse.map("1" + _)
      grayMemoizedMap.update(n, result)
      result
    }
  }

  /**
   * P50 (***) Huffman code.
   * First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman codes!
   * We suppose a set of symbols with their frequencies, given as a list of (S, F) Tuples. E.g. (("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)). Our objective is to construct a list of (S, C) Tuples, where C is the Huffman code word for the symbol S.
   *
   * scala> huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
   * res0: List[String, String] = List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100))
   */
  def huffman(ls: List[(String, Int)]): List[(String, String)] = {
    class Node(val left: Option[Node], val right: Option[Node], val value: Int, val symbol: Option[String]) {
      //      var code = "";
      def toCode(prefix: String): List[(String, String)] = {
        val leftList = left.map(_.toCode(prefix + "0")).getOrElse(Nil)
        val rightList = right.map(_.toCode(prefix + "1")).getOrElse(Nil)
        val s = symbol.map { x => List((x, prefix)) }.getOrElse(Nil)
        leftList ::: s ::: rightList
      }
    }
    val nodes = ls.map { case (x, i) => new Node(None, None, i, Some(x)) }
    def huffmanGreed(ns: List[Node]): List[Node] =
      ns.sortBy(_.value) match {
        case x :: Nil => List(x)
        case n1 :: n2 :: remain => huffmanGreed(new Node(Some(n1), Some(n2), n1.value + n2.value, None) :: remain)
      }
    //    def encode(n: Node, code: String) {
    //      n.left.map(encode(_, code + "0"))
    //      n.code = code
    //      n.right.map(encode(_, code + "1"))
    //    }

    val root = huffmanGreed(nodes).head
    //    encode(root, "")
    //    nodes.map{n =>
    //      (n.symbol.get, n.code)
    //    }
    root.toCode("")
  }

  def main(args: Array[String]) {
    table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
    println(gray(3))
    println(gray(4))
    //    println(grayMemoized(15))
    println(huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))))
  }
}

class MyBool(val a: Boolean) {
  def and(b: Boolean) = a && b
  def or(b: Boolean) = a || b
  def xor(b: Boolean) = a ^ b
}
