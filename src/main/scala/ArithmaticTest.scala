object ArithmaticTest {
  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
    val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })
  }
  class S99Int(val start: Int) {
    import S99Int._
    /**
     * P31 (**) Determine whether a given integer number is prime.
     * scala> 7.isPrime
     * res0: Boolean = true
     */
    def isPrime = {
      start match {
        case 1 => false
        case 2 => true
        case x =>
          val sqrt = scala.Math.sqrt(x).floor.toInt
          var result = true
          for (i <- 2 to sqrt if result) {
            result = x % i != 0
          }
          result
      }
    }

    def isPrime1: Boolean =
      (start > 1) && (primes takeWhile { _ <= Math.sqrt(start) } forall { start % _ != 0 })

    def isCoprimeTo(x: Int): Boolean = gcd(start, x) == 1

    //P34 Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.
    def totient: Int =
      (1 to start).filter(isCoprimeTo).length

    //P35 Construct a flat list containing the prime factors in ascending order.
    //    def primeFactors: List[Int] = {
    //      val ps = primes.takeWhile(_ <= start).toList
    //      val factor = ps.find(start % _ == 0)
    //      if (factor == None) Nil else factor.get :: (start / factor.get).primeFactors
    //    }
    def primeFactors: List[Int] = {
      def primeFactorsR(n: Int, ps: Stream[Int]): List[Int] =
        if (n.isPrime) List(n)
        else if (n % ps.head == 0) ps.head :: primeFactorsR(n / ps.head, ps)
        else primeFactorsR(n, ps.tail)
      primeFactorsR(start, primes)
    }

    //P36 Construct a list containing the prime factors and their multiplicity.
    def primeFactorMultiplicity: Map[Int, Int] = {
      primeFactors.groupBy(x => x).mapValues(_.length)
    }
    /**
     * P37 (**) Calculate Euler's totient function phi(m) (improved).
     * See problem P34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem P36 then the function phi(m>) can be efficiently calculated as follows: Let [[p1, m1], [p2, m2], [p3, m3], ...] be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
     * phi(m) = (p1-1)*p1(m1-1) * (p2-1)*p2(m2-1) * (p3-1)*p3(m3-1) * ...
     * Note that ab stands for the bth power of a.
     */
    def totientEuler: Int = start.primeFactorMultiplicity.foldLeft(1) { (r, f) =>
      f match { case (p, m) => r * (p - 1) * Math.pow(p, m - 1).toInt }
    }

    /**
     * P40 (**) Goldbach's conjecture.
     * Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers.
     * E.g. 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case.
     * It has been numerically confirmed up to very large numbers (much larger than Scala's Int can represent).
     * Write a function to find the two prime numbers that sum up to a given even integer.
     */
    def goldbach: (Int, Int) = {
      require(start > 2)
      require(start % 2 == 0)
      val ps = primes.takeWhile(_ < start)
      ps.find(x => ps.contains(start - x)) match {
        case None => throw new IllegalArgumentException
        case Some(i) => (i, start - i)
      }
    }

  }

  import S99Int._

  def gcd(x: Int, y: Int): Int = {
    if (y == 0) x else gcd(y, x % y)
  }

  def time[A](label: String)(block: => A): A = {
    val begin = System.nanoTime
    val result = block
    println(label + ": " + (System.nanoTime - begin) + "ns")
    result
  }

  def compareTotient(x: Int) {
    time("Preload primes") {
      primes.takeWhile(_ <= Math.sqrt(x))
    }
    time("totient")(x.totient)
    time("totient Euler")(x.totientEuler)
  }
  def listPrimesinRange(min: Int, max: Int): List[Int] = {
    primes.takeWhile(_ <= max).dropWhile(_ < min).toList
  }
  /**
   * P41 (**) A list of Goldbach compositions.
   * Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
   * scala> printGoldbachList(9 to 20)
   * 10 = 3 + 7
   * 12 = 5 + 7
   * 14 = 3 + 11
   * 16 = 3 + 13
   * 18 = 5 + 13
   * 20 = 3 + 17
   * In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than, say, 50. Try to find out how many such cases there are in the range 2..3000.
   *
   * Example (minimum value of 50 for the primes):
   *
   * scala> printGoldbachListLimited(1 to 2000, 50)
   * 992 = 73 + 919
   * 1382 = 61 + 1321
   * 1856 = 67 + 1789
   * 1928 = 61 + 1867
   */
  def printGoldbachList(r: Range) {
    r.filter(_ % 2 == 0).foreach { x =>
      val g = x.goldbach
      println(x + " = " + g._1 + " + " + g._2)
    }
  }
  def printGoldbachListLimited(r: Range, limit: Int) {
    r.filter(x => x > 2 && x % 2 == 0).foreach { x =>
      val g = x.goldbach
      if (g._1 > limit)
        println(x + " = " + g._1 + " + " + g._2)
    }
  }

  def main(args: Array[String]): Unit = {
    println(4.isPrime)
    println(gcd(36, 27))
    println(35.isCoprimeTo(64))
    println(10.totient)
    println(315.primeFactors)
    println(315.primeFactorMultiplicity)
    compareTotient(10090)
    println(listPrimesinRange(7, 40))
    println(22876.goldbach)
    printGoldbachList(9 to 20)
    printGoldbachListLimited(1 to 2000, 50)
  }

}