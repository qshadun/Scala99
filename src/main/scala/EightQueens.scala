object EightQueens {
  def queens(n: Int): List[List[Int]] = {
    def queensR(i: Int): List[List[Int]] = {
      if (i == 0) List(Nil)
      else for (qs <- queensR(i - 1);
                j <- 1 to n;
                if isSafe(qs, j)) yield qs :+ j
    }
    queensR(n)
  }
  
  def isSafe(qs: List[Int], t: Int): Boolean = 
    qs.zip(1 to qs.length)
      .forall(x => x._1 != t && (t - x._1).abs != (qs.length + 1 - x._2).abs)
  def main(args: Array[String]): Unit = {
    val qs = queens(8)
    println(qs)
    println(qs.length)
    println(qs.filter(x => x.head == 5))
  }

}