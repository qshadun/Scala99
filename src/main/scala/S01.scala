
import scala.collection.mutable.ListBuffer
object S01 {
  def last[T](l: List[T]): T = l match {
    case x :: Nil => x
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  def penultimate[T](l: List[T]): T = l match {
    case x :: _ :: Nil => x
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }

  def nth[T](n: Int, l: List[T]): T = (n, l) match {
    case (0, x :: _) => x
    case (x, _ :: tail) if x > 0 => nth(x - 1, tail)
    case _ => throw new NoSuchElementException

  }

  // P06
  def isPalindrome[T](ls: List[T]) = {
    if (ls.size % 2 == 1) {
      val (left, right) = ls.splitAt(ls.size / 2)
      left == right.tail.reverse
    } else false
  }

  // P07
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case xs: List[_] => flatten(xs)
    case x => List(x)
  }

  // P08
  def compress[T](ls: List[T]) = ls.foldLeft(List[T]()) { (xs, x) =>
    xs match {
      case `x` :: _ => xs
      case _ => x :: xs
    }
  }.reverse

  // P09
  def pack[T](ls: List[T]) = ls.foldLeft(List[List[T]]()) { (xs, x) =>
    xs match {
      case (head @ (`x` :: _)) :: tail => (x :: head) :: tail
      case _ => List(x) :: xs
    }
  }.reverse

  // P10
  def encode[T](ls: List[T]) = {
    pack(ls).map { xs => (xs.length, xs.head) }
  }

  // P11
  def encodeModified[T](ls: List[T]): List[Any] = {
    pack(ls).map { xs => if (xs.length == 1) xs.head else (xs.length, xs.head) }
  }

  // P12
  def decode[T](ls: List[(Int, T)]): List[T] = {
    ls.flatMap { t => List.fill(t._1)(t._2) }
  }

  // P13
  def encodeDirect[T](ls: List[T]): List[(Int, T)] = ls match {
    case Nil => Nil
    case _ =>
      val (repeatHead, tail) = ls.span { _ == ls.head }
      (repeatHead.length, repeatHead.head) :: encodeDirect(tail)
  }

  // P14
  def duplicate[T](ls: List[T]): List[T] = ls.flatMap(List.fill(2)(_))

  // P15
  def duplicateN[T](n: Int, ls: List[T]): List[T] = ls.flatMap(List.fill(n)(_))

  // P16
  //  def drop[T](n: Int, ls: List[T]): List[T] = {
  //    if (ls.length < n) ls
  //    else {
  //      val (left, right) = ls.splitAt(n)
  //      left.init ::: drop(n, right)
  //    }
  //  } 
  def drop[A](n: Int, ls: List[A]): List[A] =
    ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }

  // P20
  def removeAt[T](n: Int, ls: List[T]): (List[T], T) = {
    val (left, right) = ls.splitAt(n)
    (left ::: right.tail, right.head)
  }

  // P23
  def randomSelect[T](n: Int, ls: List[T]): List[T] = {
    if (n == 0) Nil
    else {
      val rnd = util.Random.nextInt(ls.length)
      val (remain, r) = removeAt(rnd, ls)
      r :: randomSelect(n - 1, remain)
    }
  }

  // P24
  //  def lotto(n: Int, s: Int): List[Int] = {
  //    def lottoR(n: Int, s: Int, ls: List[Int], rnd: util.Random):List[Int] = {
  //      if (n == 0) Nil
  //      else {
  //        var r = rnd.nextInt(s) + 1
  //        while (ls.contains(r)) r = rnd.nextInt(s) + 1
  //        val newls = r :: ls
  //        r :: lottoR(n - 1, s, newls, rnd)
  //      }
  //    }
  //    lottoR(n, s, List.empty[Int], util.Random)
  //  }
  def lotto(count: Int, max: Int): List[Int] =
    randomSelect(count, List.range(1, max + 1))

  // P26
  def ccombinations[T](n: Int, ls: List[T]): List[List[T]] = {
    if (n == 1) ls map { x => List(x) }
    else {
      val x = (0 to ls.length - n) flatMap { i =>
        val remain = ls.drop(i)
        val remainComb = ccombinations(n - 1, remain.tail)
        for (xs <- remainComb) yield remain.head :: xs
      }
      x.toList
    }
  }
  def flatMapSublists[A, B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case _ :: tail => f(ls) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations(n - 1, sl.tail) map { sl.head :: _ }
    }
  //
  //  def pcombinations[T](n: Int, ls: List[T]): List[List[T]] = {
  //    if (n == 1) ls map {x => List(x)}
  //    else {
  //      val x =(0 until ls.length) flatMap {i =>
  //        val (remain, x) = removeAt(i, ls)
  //        val remainComb =  combinations(n-1, remain)
  //        for (xs <- remainComb) yield x :: xs
  //      }
  //      x.toList
  //    }
  //  }
  //  def combinations[T](n: Int, ls: List[T]): List[List[T]] = {
  //    val p = pcombinations(n, ls)
  //    val buff = new ListBuffer[List[T]]
  //    p.filter{ls => 
  //      if (exists(buff, ls)) false
  //      else {
  //        buff.append(ls)
  //        true
  //      }
  //    }
  //  }
  //  def exists[T](buff: ListBuffer[List[T]], ls: List[T]) = {
  //    buff.exists(elementEqal(ls, _))
  //  }
  //  def elementEqal[T](ls1: List[T], ls2: List[T]) = ls1.length == ls2.length && ls1.forall(ls2.contains(_))
  def main(args: Array[String]): Unit = {
    //    println(last(List(1)))
    //    println(penultimate(List(1, 2, 3)))
    //    println(nth(2, List(1, 2, 3)))
    //    println(isPalindrome(List(1, 2, 3, 4, 3, 2, 1)))
    //    println(flatten(List(List(List(1, 2), List(2, 3)), 3, 4, Nil, List(2, 4))))
    //    println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    //    println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    //    println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    //    println(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    //    println(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
    //    println(encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    //    println(duplicate(List('a, 'b, 'c, 'c, 'd)))
    //    println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))
    //    println(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    //    println(removeAt(1, List('a, 'b, 'c, 'd)))
    //    println(randomSelect(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h)))
    println(lotto(6, 49))
    println(combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)))
    println(combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)).length)
  }

}