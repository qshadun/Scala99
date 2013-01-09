import S01._
object S27 {
  /**
   * P27 (**) Group the elements of a set into disjoint subsets.
   * a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
   * Example:
   *
   * scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
   * res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
   * b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
   *
   * Example:
   *
   * scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
   * res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
   * Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...). However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).
   *
   * You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
   */
  def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
    case Nil => List(Nil)
    case n :: ns => combinations(n, ls) flatMap { c =>
      group(ns, ls -- c) map { c :: _ }
    }
  }

  def lsort[A](ls: List[List[A]]): List[List[A]] = {
    ls.sortBy(_.length)
  }

  def lsortFreq[A](ls: List[List[A]]): List[List[A]] = {
    val lengthList = ls.map(_.length)
    val lengthFreq = lengthList.groupBy(x => x).mapValues(_.length)
    ls.sortBy(ns => lengthFreq(ns.length))
  }

  def lsortFreq1[A](ls: List[List[A]]): List[List[A]] = {
    val freqs = Map(encode(ls map { _.length } sort { _ < _ }) map { _.swap }: _*)
    ls sort { (e1, e2) => freqs(e1.length) < freqs(e2.length) }
  }
  
  def main(args: Array[String]) {
    //    println(group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")))
    println(lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))
    println(lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))
  }
}
