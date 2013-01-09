object KnightTour {
  case class Point(x: Int, y: Int)

  def move(n: Int, pos: Point): List[Point] = {
    def guard(p: Point) = (1 to n).contains(p.x) && (1 to n).contains(p.y)
    //    val possibleMoves = 
    //      List(Point(pos.x + 1, pos.y + 2),
    //        	 Point(pos.x + 2, pos.y + 1),
    //        	 Point(pos.x - 1, pos.y + 2),
    //        	 Point(pos.x - 2, pos.y + 1),
    //        	 Point(pos.x + 1, pos.y - 2),
    //        	 Point(pos.x + 2, pos.y - 1),
    //        	 Point(pos.x - 1, pos.y - 2),
    //        	 Point(pos.x - 2, pos.y - 1))
    val pm = for (
      i <- List(1, 2); j = List(1, 2).filter(_ != i).head;
      signx <- List(true, false); dx = if (!signx) (-1) * i else i;
      signy <- List(true, false); dy = if (!signy) (-1) * j else j
    ) yield Point(pos.x + dx, pos.y + dy)
    pm.filter(guard)
  }

  def tour(n: Int, pos: Point): List[Point] = {
    val allPos = for (i <- 1 to n; j <- 1 to n) yield Point(i, j)
    def tourR(visited: List[Point]): List[Point] = {
      if (allPos.forall(visited.contains(_))) visited
      else {
        val nextMove = move(n, visited.head).find(! visited.contains(_)).getOrElse(
	            visited.find(move(n, _).exists(!visited.contains(_))).get //Retro back
            )
        tourR(nextMove :: visited)
      }
    }
    tourR(List(pos)).reverse
  }
  
  def tourLazy(n: Int, pos: Point): Stream[List[Point]] = {
    var result = Stream.empty
    val allPos = for (i <- 1 to n; j <- 1 to n) yield Point(i, j)
    result
  } 
  
  def main(args: Array[String]): Unit = {
    val t = tour(8, Point(1, 1))
    println(t)
    println(t.length)
  }

}