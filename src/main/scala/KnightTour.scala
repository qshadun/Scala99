object KnightTour {
  case class Point(x: Int, y: Int) {
    def +(o: Point) = Point(x + o.x, y + o.y)
    def -(o: Point) = Point(x - o.x, y - o.y)
    def +(t: (Int, Int)) = Point(x + t._1, y + t._2)
    def -(t: (Int, Int)) = Point(x - t._1, y - t._2)
    def jumps(n: Int): List[Point] =
      List((2, 1), (1, 2), (-1, 2), (-2, 1)) flatMap { x =>
        List(this + x, this - x)
      } filter { p =>
        (p.x min p.y) >= 1 && (p.x max p.y) <= n
      }
  }

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
        val nextMove = move(n, visited.head).find(!visited.contains(_)).getOrElse(
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
    // TODO: Not finished
    result
  }

  // Utility function.
  // Filters out the positions already seen in the given set, and orders the
  // points with the one with the fewest possibilities first, in line with
  // Warnsdorff's heuristic.
  def jumps(p: Point, n: Int, s: Set[Point]): List[Point] = {
    def filtered(p: Point) = p.jumps(n).remove(s(_))
    filtered(p).sort(filtered(_).length < filtered(_).length)
  }

  // Find one tour.
  // Because we're using the stack for our state here, one of the simplest ways
  // to signal a result is to throw an exception and not worry about properly
  // unwinding the stack.
  class TourFound(val tour: List[Point]) extends Exception
  def knightsTour(n: Int): List[Point] = knightsTour(n, Point(1, 1), (p, s) => s.size == n * n)
  def knightsTourClosed(n: Int): List[Point] =
    knightsTour(n, Point(1, 1), (p, s) => s.size == n * n && p.jumps(n).contains(Point(1, 1)))
  def knightsTour(n: Int, start: Point, done: (Point, Set[Point]) => Boolean): List[Point] =
    try {
      findPath(n, start, Set(start), List(start), done)
      Nil
    } catch {
      case t: TourFound => t.tour
    }
  def findPath(n: Int, p: Point, s: Set[Point], soFar: List[Point], done: (Point, Set[Point]) => Boolean): Unit =
    if (done(p, s)) throw new TourFound(soFar)
    else jumps(p, n, s).foreach(q => findPath(n, q, s + q, q :: soFar, done))

  // Find all tours
  def knightsTourComplete(n: Int): List[List[Point]] = knightsTourComplete(n, Point(1, 1))
  def knightsTourCompleteClosed(n: Int): List[List[Point]] =
    knightsTourComplete(n, Point(1, 1)).filter(_.head.jumps(n).contains(Point(1, 1)))
  def knightsTourComplete(n: Int, start: Point): List[List[Point]] =
    findpathComplete(n, start, Set(start), List(start))

  def findpathComplete(n: Int, p: Point, s: Set[Point], soFar: List[Point]): List[List[Point]] =
    if (s.size == n * n) List(soFar)
    else jumps(p, n, s).flatMap(q => findpathComplete(n, q, s + q, q :: soFar))

  // Find all tours, lazily.
  // Instead of implicitly embedding our state into the stack, here we
  // explicitly keep a list containing what were stack frames in the other
  // versions.  Also, rather than mapping across a list in each function call,
  // we just make a frame for each potential destination and drop each one onto
  // our imitation stack.
  case class Frame(n: Int, p: Point, s: Set[Point], soFar: List[Point])
  def knightsTourLazy(n: Int): Stream[List[Point]] = knightsTourLazy(n, Point(1, 1))
  def knightsTourLazyClosed(n: Int): Stream[List[Point]] =
    knightsTourLazy(n, Point(1, 1)).filter(_.head.jumps(n).contains(Point(1, 1)))
  def knightsTourLazy(n: Int, start: Point): Stream[List[Point]] =
    nextTour(List(Frame(n, start, Set(start), List(start))))
  def nextTour(stack: List[Frame]): Stream[List[Point]] = stack match {
    case Nil => Stream.empty
    case Frame(n, p, s, soFar) :: tail =>
      if (s.size == n * n) Stream.cons(soFar, nextTour(tail))
      else nextTour(jumps(p, n, s).map(q => Frame(n, q, s + q, q :: soFar)) ::: tail)
  }
  def main(args: Array[String]): Unit = {
    //    val t = tour(8, Point(1, 1))
    //    println(t)
    //    println(t.length)

    println(knightsTourLazyClosed(8).take(1).toList.mkString("\n"))
  }

}