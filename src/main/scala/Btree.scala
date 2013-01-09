object Btree {
  sealed abstract class Tree[+T] {
    /**
     * P56 (**) Symmetric binary trees.
     * Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right subtree is the mirror image of the left subtree. Add an isSymmetric method to the Tree class to check whether a given binary tree is symmetric. Hint: Write an isMirrorOf method first to check whether one tree is the mirror image of another. We are only interested in the structure, not in the contents of the nodes.
     * scala> Node('a', Node('b'), Node('c')).isSymmetric
     * res0: Boolean = true
     */

    def isMirrorOf[V](tree: Tree[V]): Boolean
    def isSymmetric: Boolean
    //P57 (**) Binary search trees (dictionaries).
    def addValue[U >: T <% Ordered[U]](i: U): Tree[U]

    def getNodeNumber: Int
    def leafCount: Int
    def leafList: List[T]
    def internalList: List[T]
    def atLevel(n: Int): List[T]
    //P64 (**) Layout a binary tree (1).
    def layoutBinaryTree: Tree[T]
    def toInOrderList: List[Tree[T]]
    def layoutBinaryTree1: Tree[T] = layoutBinaryTreeInternal(1, 1)._1
    def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int)
    // P65
    def layoutBinaryTree2: Tree[T] = {
      val rootX = (0 to (getLeftMostDepth - 2)).foldLeft(0) { _ + Math.pow(2, _).toInt } * Math.pow(2, getHeight - getLeftMostDepth).toInt + 1
      layoutBinaryTree2Internal(rootX, 1, getHeight)
    }
    def layoutBinaryTree2Internal(x: Int, depth: Int, height: Int): Tree[T]
    def getHeight: Int
    def getLeftMostDepth: Int
    // P66
    //    def leftMostPath: List[Tree[T]]
    //    def rightMostPath: List[Tree[T]]
    //    def childGap3: Int
    //    def layoutBinaryTree3: Tree[T] = {
    //      val rootX = leftMostPath.reverse.foldLeft(0) {(sum, t) => 
    //        sum + t.childGap3
    //      }
    //      layoutBinaryTree3Internal(rootX, 1)
    //    }
    //    def layoutBinaryTree3Internal(x: Int, depth: Int): Tree[T]
    def bounds: List[(Int, Int)]
    def layoutBinaryTree3: Tree[T] =
      layoutBinaryTree3Internal(bounds.map(_._1).reduceLeft(_ min _) * -1 + 1, 1)
    def layoutBinaryTree3Internal(x: Int, depth: Int): Tree[T]
    // P67
    def toCharStr: String

    //P68
    def preOrder: List[T]
    def inOrder: List[T]

    //P69
    def toDotString: String
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    override def isMirrorOf[V](tree: Tree[V]): Boolean = tree match {
      case t: Node[V] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)
      case _ => false
    }
    override def isSymmetric: Boolean = left.isMirrorOf(right)

    override def addValue[U >: T <% Ordered[U]](i: U): Tree[U] = {
      if (i < value) Node(value, left.addValue(i), right)
      else Node(value, left, right.addValue(i))
    }

    override def getNodeNumber = left.getNodeNumber + right.getNodeNumber + 1

    override def leafCount =
      if (left == End && right == End) 1
      else left.leafCount + right.leafCount
    override def leafList = (left, right) match {
      case (End, End) => List(value)
      case _ => left.leafList ::: right.leafList
    }
    override def internalList = (left, right) match {
      case (End, End) => Nil
      case _ => value :: left.internalList ::: right.internalList
    }
    override def atLevel(n: Int) = n match {
      case n if n < 1 => Nil
      case 1 => List(value)
      case _ => left.atLevel(n - 1) ::: right.atLevel(n - 1)
    }

    override def toInOrderList = left.toInOrderList ::: List(this) ::: right.toInOrderList
    override def layoutBinaryTree: Tree[T] = {
      def layout(t: Tree[T], ls: List[Tree[T]], h: Int): Tree[T] = t match {
        case End => End
        case node @ Node(v, left, right) => new PositionedNode(v, layout(left, ls, h + 1), layout(right, ls, h + 1), ls.indexOf(node) + 1, h)
      }
      layout(this, toInOrderList, 1)
    }
    def layoutBinaryTreeInternal(x: Int, depth: Int): (Tree[T], Int) = {
      val (leftTree, myX) = left.layoutBinaryTreeInternal(x, depth + 1)
      val (rightTree, nextX) = right.layoutBinaryTreeInternal(myX + 1, depth + 1)
      (PositionedNode(value, leftTree, rightTree, myX, depth), nextX)
    }
    override def layoutBinaryTree2Internal(x: Int, depth: Int, height: Int) = {
      val gap = Math.pow(2, height - depth - 1).toInt
      val leftTree = left.layoutBinaryTree2Internal(x - gap, depth + 1, height)
      val rightTree = right.layoutBinaryTree2Internal(x + gap, depth + 1, height)
      new PositionedNode(value, leftTree, rightTree, x, depth)
    }
    override def getHeight = Math.max(left.getHeight, right.getHeight) + 1
    override def getLeftMostDepth = left.getLeftMostDepth + 1
    //
    //    override def leftMostPath = this :: left.leftMostPath
    //    override def rightMostPath = this :: right.rightMostPath
    //    override def childGap3 = {
    //      val leftGap = left.rightMostPath.foldLeft(0) { _ + _.childGap3 }
    //      val rightGap = right.leftMostPath.foldLeft(0) { _ + _.childGap3 }
    //      Math.max(1, (leftGap + rightGap + 1) / 2)
    //    }
    //
    //    override def layoutBinaryTree3Internal(x: Int, depth: Int) = {
    //      val leftTree = left.layoutBinaryTree3Internal(x - childGap3, depth + 1)
    //      val rightTree = right.layoutBinaryTree3Internal(x + childGap3, depth + 1)
    //      new PositionedNode(value, leftTree, rightTree, x, depth)
    //    }
    def bounds: List[(Int, Int)] = {
      def lowerBounds = (left.bounds, right.bounds) match {
        case (Nil, Nil) => Nil
        case (lb, Nil) => lb.map((b) => (b._1 - 1, b._2 - 1))
        case (Nil, rb) => rb.map((b) => (b._1 + 1, b._2 + 1))
        case (lb, rb) => {
          val shift = lb.zip(rb).map((e) => (e._1._2 - e._2._1) / 2 + 1).reduceLeft(_ max _)
          lb.map(Some(_)).zipAll(rb.map(Some(_)), None, None).map(_ match {
            case (Some((a, b)), Some((c, d))) => (a - shift, d + shift)
            case (Some((a, b)), None) => (a - shift, b - shift)
            case (None, Some((c, d))) => (c + shift, d + shift)
            case (None, None) => throw new Exception // Placate the compiler; can't get here.
          })
        }
      }
      (0, 0) :: lowerBounds
    }
    def layoutBinaryTree3Internal(x: Int, depth: Int): Tree[T] = bounds match {
      case _ :: (bl, br) :: _ => PositionedNode(
        value, left.layoutBinaryTree3Internal(x + bl, depth + 1),
        right.layoutBinaryTree3Internal(x + br, depth + 1), x, depth)
      case _ => PositionedNode(value, End, End, x, depth)
    }

    override def toCharStr = (left, right) match {
      case (End, End) => value.toString
      case _ => value + "(" + left.toCharStr + "," + right.toCharStr + ")"
    }

    def preOrder: List[T] = List(value) ::: left.preOrder ::: right.preOrder
    def inOrder: List[T] = left.inOrder ::: List(value) ::: right.inOrder

    def toDotString = value + left.toDotString + right.toDotString
  }

  case class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends Node[T](value, left, right) {
    override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  case object End extends Tree[Nothing] {
    override def toString = "."
    override def isMirrorOf[V](tree: Tree[V]): Boolean = tree == End
    override def isSymmetric: Boolean = true
    override def addValue[U <% Ordered[U]](i: U) = Node(i)
    override def getNodeNumber = 0
    override def leafCount = 0
    override def leafList = Nil
    override def internalList = Nil
    override def atLevel(n: Int) = Nil
    override def toInOrderList = Nil
    override def layoutBinaryTree = End
    override def layoutBinaryTreeInternal(x: Int, depth: Int) = (End, x)
    override def layoutBinaryTree2Internal(x: Int, depth: Int, height: Int) = End
    override def getHeight = 0
    override def getLeftMostDepth = 0
    //    override def leftMostPath = Nil
    //    override def rightMostPath = Nil
    //    override def childGap3 = 0
    //    override def layoutBinaryTree3Internal(x: Int, depth: Int) = End
    override def bounds: List[(Int, Int)] = Nil
    override def layoutBinaryTree3Internal(x: Int, depth: Int) = End
    override def toCharStr = ""

    def preOrder = Nil
    def inOrder = Nil

    def toDotString = "."

  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }
  // P55 (**) Construct completely balanced binary trees.
  //     In a completely balanced binary tree, the following property holds for
  //     every node: The number of nodes in its left subtree and the number of
  //     nodes in its right subtree are almost equal, which means their difference
  //     is not greater than one. 
  //    
  //     Define an object named Tree.  Write a function Tree.cBalanced to
  //     construct completely balanced binary trees for a given number of nodes.
  //     The function should generate all solutions.  The function should take as
  //     parameters the number of nodes and a single value to put in all of them.
  //
  //     scala> Tree.cBalanced(4, "x")
  //     res0: List(Node[String]) = List(T(x T(x . .) T(x . T(x . .))), T(x T(x . .) T(x T(x . .) .)), ...
  object Tree {
    def cBalanced[T](n: Int, s: T): List[Tree[T]] = {
      def cTree[U](k: Int, x: U): List[Tree[U]] = k match {
        case 0 => List(End)
        case 1 => List(Node(x))
        case _ => (0 to k - 1).flatMap { i =>
          for (left <- cTree(i, x); right <- cTree(k - i - 1, x)) yield Node(x, left, right)
        }.toList
      }
      n match {
        case 0 => List(End)
        case _ if n % 2 == 0 =>
          val t1 = cTree(n / 2, s)
          val t2 = cTree(n / 2 - 1, s)
          val tt1 = for (left <- t1; right <- t2) yield Node(s, left, right)
          val tt2 = for (left <- t2; right <- t1) yield Node(s, left, right)
          tt1 ::: tt2
        case _ =>
          val t = cTree(n / 2, s)
          for (left <- t; right <- t) yield Node(s, left, right)
      }
    }

    def fromList[U <% Ordered[U]](ls: List[U]): Tree[U] =
      ls.foldLeft(End: Tree[U]) { _ addValue _ }

    def symmetricBalancedTrees(n: Int, s: String): List[Tree[String]] = {
      cBalanced(n, s).filter(_.isSymmetric)
    }

    /**
     * P59 (**) Construct height-balanced binary trees.
     * In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.
     * Write a method Tree.hbalTrees to construct height-balanced binary trees for a given height with a supplied value for the nodes. The function should generate all solutions.
     *
     * scala> Tree.hbalTrees(3, "x")
     * res0: List[Node[String]] = List(T(x T(x T(x . .) T(x . .)) T(x T(x . .) T(x . .))), T(x T(x T(x . .) T(x . .)) T(x T(x . .) .)), ...
     */
    def hbalTrees[T](n: Int, s: T): List[Tree[T]] = n match {
      case 0 => List(End)
      case 1 => List(Node(s))
      case _ =>
        val minusOne = hbalTrees(n - 1, s)
        val minusTwo = hbalTrees(n - 2, s)
        val leftLower = for (left <- minusTwo; right <- minusOne) yield Node(s, left, right)
        val equal = for (left <- minusOne; right <- minusOne) yield Node(s, left, right)
        val rightLower = for (left <- minusOne; right <- minusTwo) yield Node(s, left, right)
        leftLower ::: equal ::: rightLower
    }

    def minHbalNodes(height: Int): Int = height match {
      case 0 => 0
      case 1 => 1
      case _ => minHbalNodes(height - 1) + minHbalNodes(height - 2) + 1
    }
    def maxHbalNodes(height: Int): Int = Math.pow(2, height).toInt - 1

    def minHbalHeight(n: Int): Int = n match {
      case 0 => 0
      case _ => minHbalHeight(n / 2) + 1
    }
    def maxHbalHeight(nodes: Int): Int =
      Stream.from(1).takeWhile(minHbalNodes(_) <= nodes).last

    def hbalTreesWithNodes[T](n: Int, x: T): List[Tree[T]] = {
      (minHbalHeight(n) to maxHbalHeight(n)).flatMap {
        hbalTrees(_, x).filter(_.getNodeNumber == n)
      }.toList
    }

    def completeBinaryTreeHeight(nodes: Int) =
      Stream.from(1).takeWhile(maxHbalNodes(_) <= nodes).last
    def completeBinaryTree[T](n: Int, x: T): Tree[T] = n match {
      case 0 => End
      case 1 => Node(x)
      case _ =>
        val h = completeBinaryTreeHeight(n)
        val lastLevels = n - maxHbalNodes(h)
        val rightCount =
          if (lastLevels <= h) maxHbalNodes(h - 1)
          else maxHbalNodes(h - 1) + lastLevels - h
        Node(x, completeBinaryTree(n - rightCount - 1, x), completeBinaryTree(rightCount, x))
    }

    def completeBinaryTree1[T](nodes: Int, value: T): Tree[T] = {
      def generateTree(addr: Int): Tree[T] =
        if (addr > nodes) End
        else Node(value, generateTree(2 * addr), generateTree(2 * addr + 1))
      generateTree(1)
    }

    def fromString(s: String): Tree[Char] = {
      def findComma(sub: String): Int = {
        var result = 0
        var parentheseDepth = 0
        var found = false
        for (i <- 0 until sub.length if !found)
          sub.charAt(i) match {
            case ',' if parentheseDepth == 0 => result = i; found = true
            case '(' => parentheseDepth += 1
            case ')' => parentheseDepth -= 1
            case _ =>
          }
        result
      }
      val treeRegex = """([a-zA-z])\((.*)\)""".r
      if (s.length == 1) Node(s.charAt(0))
      else {
        val treeRegex(value, sub) = s
        val mid = findComma(sub)
        val left = if (mid == 0) End else fromString(sub.substring(0, mid))
        val right = if (mid < sub.length - 1) fromString(sub.substring(mid + 1)) else End
        new Node(value.charAt(0), left, right)
      }
    }

    def preInTree[T](pre: List[T], in: List[T]): Tree[T] = pre match {
      case Nil => End
      case value :: preTail =>
        val (leftIn, _ :: rightIn) = in.span(_ != value)
        val (leftPre, rightPre) = preTail.splitAt(leftIn.length)
        new Node(value, preInTree(leftPre, leftIn), preInTree(rightPre, rightIn))
    }

    def fromDotString(s: String): Tree[Char] = s.length match {
      case 0 => End
      case 1 => End
      case _ =>
        val value = s(0)
        val rightStartIndex =
          if (s(1) == '.') 2
          else {
            var pos = 2
            var dotNumber = 2
            for (i <- 2 until s.length if dotNumber > 0) {
              pos += 1
              if (s(i) == '.') dotNumber -= 1 else dotNumber += 1
            }
            pos
          }
        new Node(s(0), fromDotString(s.substring(1, rightStartIndex)), fromDotString(s.substring(rightStartIndex)))
    }

    def fromDotstring1(ds: String): Tree[Char] = {
      def fromDotstringR(pos: Int): (Tree[Char], Int) = ds(pos) match {
        case '.' => (End, pos + 1)
        case c => {
          val (lTree, lPos) = fromDotstringR(pos + 1)
          val (rTree, rPos) = fromDotstringR(lPos)
          (Node(c, lTree, rTree), rPos)
        }
      }
      fromDotstringR(0)._1
    }
  }

  def main(args: Array[String]): Unit = {
    //    val t = Node('a',
    //      Node('b', Node('d'), Node('e')),
    //      Node('c', End, Node('f', Node('g'), End)))
    //    println(t.toString)
    //    println(Tree.cBalanced(5, "x"))
    //    println(Node('a', Node('b', Node('d'), End), Node('c', End, Node('e'))).isSymmetric)
    //    println(Tree.fromList(List(3, 2, 5, 7, 1)))
    //    println(Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric)
    //    println(Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric)
    //    println(Tree.symmetricBalancedTrees(5, "x"))
    //    println(Tree.hbalTrees(3, "x"))
    //    println(Tree.minHbalNodes(3))
    //    println(Tree.maxHbalHeight(5))
    //    println(Tree.maxHbalNodes(4))
    //    println(Tree.hbalTreesWithNodes(4, "x"))
    //    println(Tree.hbalTreesWithNodes(15, "x").length)
    //    println(t.leafCount)
    //    println(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList)
    //    println(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2))
    //    println(Tree.completeBinaryTree1(8, "x"))
    //    println(Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree)
    //    println(Tree.fromList(List('n', 'k', 'm', 'c', 'a', 'h', 'g', 'e', 'u', 'p', 's', 'q')).layoutBinaryTree1)
    //    println(Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree2)
    //    println(Tree.fromList(List('n', 'k', 'm', 'c', 'a', 'e', 'd', 'g', 'u', 'p', 'q')).layoutBinaryTree2)
    //    println(Node('a', Node('b', End, Node('c', End, Node('e'))), Node('d')).layoutBinaryTree3)
    //    println(Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))).toCharStr)
    //    println(Tree.fromString("a(b(d,e),c(,f(g,)))"))

    println(Tree.fromString("a(b(d,e),c(,f(g,)))").preOrder)
    println(Tree.fromString("a(b(d,e),c(,f(g,)))").inOrder)
    println(Tree.preInTree(List('a', 'b', 'd', 'e', 'c', 'f', 'g'), List('d', 'b', 'e', 'a', 'c', 'g', 'f')).toCharStr)

    println(Tree.fromString("a(b(d,e),c(,f(g,)))").toDotString)
    println(Tree.fromDotString("abd..e..c.fg...").toCharStr)
  }
}