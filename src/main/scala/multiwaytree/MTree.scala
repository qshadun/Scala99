package multiwaytree
import scala.collection.mutable.ListBuffer

case class MTree[+T](value: T, children: List[MTree[T]]) {
  def this(value: T) = this(value, List())
  //  override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"

  def nodeCount: Int = 1 + children.map(_.nodeCount).sum

  override def toString = value + children.map { _.toString + "^" }.mkString

  //  def internalPathLength: Int = internalPathLengthList.sum
  //  
  //  def internalPathLengthList: List[Int] = {
  //    children.map(x => 1) ::: children.flatMap(_.internalPathLengthList.map(1 + ))
  //  }
  def internalPathLength: Int =
    children.foldLeft(0)((r, c) => r + c.nodeCount + c.internalPathLength)

  def postorder: List[T] = children.flatMap(_.postorder) ::: List(value)

  def lispyTree: String =
    children.length match {
      case 0 => value.toString
      case _ => "(" + value + " " + children.map(_.lispyTree).mkString(" ") + ")"
    }
}

object MTree {
  def apply[T](value: T) = new MTree(value, List())
  def string2MTree2(s: String): MTree[Char] = {
    val cs = ListBuffer.empty[String]
    var start = 1
    var backtrack = 0
    for (i <- 1 until s.length())
      s(i) match {
        case '^' =>
          backtrack -= 1
          if (backtrack == 0) {
            cs += s.substring(start, i)
            start = i + 1
          }
        case _ => backtrack += 1
      }
    new MTree(s(0), cs.toList.map(string2MTree))
  }

  implicit def string2MTree(s: String): MTree[Char] = {
    def nextStrBound(pos: Int, nesting: Int): Int =
      if (nesting == 0) pos
      else nextStrBound(pos + 1, if (s(pos) == '^') nesting - 1 else nesting + 1)
    def splitChildStrings(pos: Int): List[String] =
      if (pos >= s.length) Nil
      else {
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end - 1) :: splitChildStrings(end)
      }
    MTree(s(0), splitChildStrings(1).map(string2MTree(_)))
  }

  def fromLispyTree(s: String): MTree[Char] = {
    def nextStrBound(pos: Int, nesting: Int): Int =
      if (nesting == 0) pos
      else nextStrBound(pos + 1, setNesting(s(pos), nesting))
      
    def setNesting(c: Char, n: Int) = c match {
      case '(' => n + 1
      case ')' => n - 1
      case _ => n
    }
    
    def splitChildStrings(pos: Int): List[String] = s(pos) match {
      case ')' => Nil
      case '(' =>
        val end = nextStrBound(pos + 1, 1)
        s.substring(pos, end) :: splitChildStrings(end)
      case ' ' => splitChildStrings(pos + 1)
      case _ => s(pos).toString :: splitChildStrings(pos + 1)
    }
    s.length match {
      case 1 => MTree(s(0))
      case _ =>
        new MTree(s(1), splitChildStrings(3).map(fromLispyTree).toList)
    }
  }
}

import MTree._
object TestMTree {
  def main(args: Array[String]) {
//    println(MTree('a', List(MTree('f', List(MTree('o', List(MTree('p'), MTree('q'))))), MTree('b'))).nodeCount)
//    println(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).toString)
//    println(MTree.string2MTree("afg^^c^bd^e^^"))
//    println("afg^^c^bd^e^^".internalPathLength)
//    println("afg^^c^bd^e^^".postorder)
//    println("afg^^c^bd^e^^".lispyTree)
    println(fromLispyTree("(a (f (g h)) c (b d e))"))
  }
}