package graph

abstract class GraphBase[T, U] {
  case class Edge(n1: Node, n2: Node, value: U) {
    def toTuple = (n1.value, n2.value, value)
  }
  case class Node(value: T) {
    var adj: List[Edge] = Nil
    // neighbors are all nodes adjacent to this node.
    def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)

    // partners are all nodes either adjacent to this node or to which this
    // node is adjacent.
    def partners: List[Node] =
      edges.map(edgePartner(_, this)).remove(_.isEmpty).map(_.get).removeDuplicates

    def degree = neighbors.length

    def nodesByDepth(seen: Set[Node]): List[Node] = {
      def nodesByDepthR(neighbors: List[Node], s: Set[Node]): List[Node] =
        neighbors match {
          case Nil => Nil
          case n :: tail if s(n) => nodesByDepthR(tail, s)
          case n :: tail => {
            val subnodes = n.nodesByDepth(s)
            subnodes ::: nodesByDepthR(tail, s ++ subnodes)
          }
        }
      nodesByDepthR(neighbors, seen + this) ::: List(this)
    }
  }

  var nodes: Map[T, Node] = Map()
  var edges: List[Edge] = Nil

  // If the edge E connects N to another node, returns the other node,
  // otherwise returns None.
  def edgeTarget(e: Edge, n: Node): Option[Node]

  override def equals(o: Any) = o match {
    case g: GraphBase[_, _] => (nodes.keys.toList -- g.nodes.keys.toList == Nil &&
      edges.map(_.toTuple) -- g.edges.map(_.toTuple) == Nil)
    case _ => false
  }
  def addNode(value: T) = {
    val n = new Node(value)
    nodes = Map(value -> n) ++ nodes
    n
  }

  def loneNodes: List[Node] = {
    val connectedNodes = edges.foldLeft(Set.empty[Node])((s, e) => s + e.n1 + e.n2)
    nodes.values.filter(!connectedNodes(_)).toList
  }

  def edgeSymbol: String

  override def toString() = {
    val edgesStringList = edges.map(e => e.n1.value + edgeSymbol + e.n2.value + (if (e.value == ()) "" else "/" + e.value))
    val loneNodesStringList = loneNodes.map(_.value.toString)
    "[" + (edgesStringList ++ loneNodesStringList).mkString(",") + "]"
  }
  override def hashCode = nodes.values.map(_.hashCode).sum * 37 + edges.map(_.hashCode).sum

  def toTermForm: (List[T], List[(T, T, U)]) = (nodes.keySet.toList, edges.map(_.toTuple))

  def toAdjacentForm: List[(T, List[(T, U)])] = nodes.keys.map(v =>
    (v, for (e <- edges; t <- edgeTarget(e, nodes(v))) yield (t.value, e.value))).toList

  def findPaths(start: T, end: T): List[List[T]] = {
    def findPathR(soFar: List[T]): List[List[T]] = nodes(soFar.head).neighbors.filter(x => x.value == end || !soFar.contains(x.value)).flatMap {
      case Node(`end`) => List((end :: soFar).reverse)
      case Node(n) => findPathR(n :: soFar)
    }
    findPathR(List(start))
  }

  def findCycles(n: T): List[List[T]] = findPaths(n, n).filter(_.length > 3)

  def hasCycles: Boolean = nodes.keys.exists(!findCycles(_).isEmpty)

  def isIsomorphicTo[R, S](o: GraphBase[R, S]): Boolean = {
    // Build a lazy list so we only have to evaluate as much as necessary.
    def listMappings(tNodes: List[Node], oNodes: List[o.Node]) =
      tNodes.view.flatMap(tn => oNodes.view.map((tn, _)))
    // Used on partially-filled isomorphisms to weed out some early.
    def isValidMapping(iso: Map[Node, o.Node]): Boolean =
      nodes.values forall { tn =>
        (!iso.contains(tn) ||
          tn.neighbors.filter(iso.contains).forall(tnn => iso(tn).neighbors.contains(iso(tnn))))
      }
    def isValidCompleteMapping(iso: Map[Node, o.Node]): Boolean =
      nodes.values forall { tn =>
        Set(tn.neighbors.map(iso): _*) == Set(iso(tn).neighbors: _*)
      }
    def isIsomorphicToR(tNodes: List[Node], oNodes: List[o.Node], iso: Map[Node, o.Node]): Boolean =
      if (tNodes == Nil) isValidCompleteMapping(iso)
      else listMappings(tNodes, oNodes).filter(p => isValidMapping(iso + p)) exists { p =>
        isIsomorphicToR(tNodes - p._1, oNodes - p._2, iso + p)
      }
    isIsomorphicToR(nodes.values.toList, o.nodes.values.toList, Map())
  }

  // My not efficient implementation
  def isIsomorphicTo1[A, B](g: Graph[A, B]): Boolean = {
    val morph = bijection(g).filter(b => {
      edges.forall(e => g.nodes(b(e.n1.value)).neighbors.exists(n => n.value == b(e.n2.value)))
    })
    println(morph)
    morph.length > 0
  }

  def bijection[A, B](g: Graph[A, B]): List[Map[T, A]] = {
    def bijectionR(l1: List[T], l2: List[A]): List[Map[T, A]] = {
      if (l1.length == nodes.size) List(Map(l1.zip(l2): _*))
      else {
        val av1 = nodes.keys.filter(!l1.contains(_))
        val av2 = g.nodes.keys.filter(!l2.contains(_))
        av2.flatMap(x => bijectionR(av1.head :: l1, x :: l2)).toList
      }
    }
    bijectionR(Nil, Nil)
  }

  def nodesByDegree: List[Node] = nodes.values.toList.sortBy(_.degree).reverse

  def colorNodes: List[(Node, Int)] = {
    def colorNodesR(sofar: Map[Node, Int], remain: List[Node]): List[(Node, Int)] = {
      if (remain == Nil) sofar.toList
      else {
        val ch = remain.head
        val color = Stream.from(1).filter(col => {
          ch.neighbors.forall(n => !sofar.contains(n) || sofar(n) != col)
        }).take(1).head
        colorNodesR(sofar + (ch -> color), remain.tail)
      }
    }
    colorNodesR(Map.empty[Node, Int], nodesByDegree)
  }

  // My implementation, non functional
  def nodesByDepthFrom1(t: T): List[T] = {
    var visited = List.empty[T]
    def depthR(stack: List[T]): Unit = {
      if (stack == Nil) ()
      else {
        nodes(stack.head).neighbors.foreach { n =>
          if (stack.contains(n.value) || visited.contains(n.value)) ()
          else depthR(n.value :: stack)
        }
        visited = stack.head :: visited
      }
    }
    depthR(List(t))
    visited.reverse
  }

  def nodesByDepthFrom(start: T): List[T] =
    nodes(start).nodesByDepth(Set()).map(_.value)

  // bipartite = can color all nodes use only 2 color
  def isBipartite1: Boolean = colorNodes.forall(x => x._2 <= 2)

  def isBipartiteInternal = {
    def isBipartiteR(evenToCheck: List[Node], oddToCheck: List[Node], evenSeen: Set[Node], oddSeen: Set[Node]): Boolean =
      (evenToCheck, oddToCheck) match {
        case (Nil, Nil) => true
        case (e :: eTail, odd) =>
          e.partners.forall(!evenSeen(_)) && isBipartiteR(eTail, odd.union(e.partners.remove(oddSeen(_))), evenSeen + e, oddSeen ++ e.partners)
        case (Nil, o :: oTail) =>
          o.partners.forall(!oddSeen(_)) && isBipartiteR(o.partners.remove(oddSeen(_)), oTail, evenSeen ++ o.partners, oddSeen + o)
      }
    isBipartiteR(List(nodes.values.head), Nil, Set(), Set())
  }
  def isBipartite: Boolean = {
    nodes.isEmpty || splitGraph.forall(_.isBipartiteInternal)
  }

  //  import scala.collection.mutable
  //  def isBipartite: Boolean = {
  //    case class ThirdColorException(n: Node) extends Exception 
  //    def colorNode(current: Node, color: Int, colored: mutable.Map[Node, Int]) {
  //      colored(current) = color
  //      colored.filter(x => x._2 == color).foreach {x =>
  //        if (x._1.neighbors.contains(current) || current.neighbors.contains(x._1)) throw new ThirdColorException(current)
  //      }
  //      if (colored.size == nodes.size) return
  //      else {
  //        current.neighbors.filter(!colored.contains(_)).foreach {n =>
  //          colorNode(n, (color + 1)%2, colored)
  //        }
  //        nodes.values.filter(!colored.contains(_)) match{
  //          case h :: _ => colorNode(h, 0, colored) 
  //          case Nil =>
  //        }
  //      }
  //    }
  //    
  //    try {
  //      val colorMap = mutable.Map.empty[Node, Int]
  //      colorNode(nodes.values.head, 0, colorMap)
  //      println(colorMap)
  //      true
  //    } catch {
  //      case e: ThirdColorException => println(e.n); false
  //    }
  //  }

  // If node N is a member of edge E, returns the other member of the edge.
  // This differs from edgeTarget in that if it is given an edge in a directed
  // graph and N is the target of that edge, it will still return the source
  // node for the edge.
  def edgePartner(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else if (e.n2 == n) Some(e.n1)
    else None

  def splitGraph: List[GraphBase[T, U]] = {
    def nodes2Graph(nodes: List[Node]): GraphBase[T, U] = {
      val adjacentForm = nodes.map(n => (n.value, n.adj.map(e =>
        (edgeTarget(e, n).get.value, e.value))))
      this match {
        case _: Graph[_, _] => Graph.adjacentLabel(adjacentForm)
        case _: Digraph[_, _] => Digraph.adjacentLabel(adjacentForm)
      }
    }
    def findConnectedNodes(candidates: List[Node], soFar: List[Node]): List[Node] =
      candidates match {
        case Nil => soFar
        case n :: tail => {
          val newNodes = n.partners -- soFar - n
          findConnectedNodes(tail.union(newNodes), n :: soFar)
        }
      }
    def splitGraphR(unsplit: List[Node]): List[GraphBase[T, U]] = unsplit match {
      case Nil => Nil
      case n :: tail => {
        val connectedNodes = findConnectedNodes(List(n), Nil)
        nodes2Graph(connectedNodes) :: splitGraphR(unsplit -- connectedNodes)
      }
    }
    splitGraphR(nodes.values.toList)
  }
}

class Graph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Graph[_, _] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else if (e.n2 == n) Some(e.n1)
    else None

  def addEdge(n1: T, n2: T, value: U) = {
    val e = new Edge(nodes(n1), nodes(n2), value)
    edges = e :: edges
    nodes(n1).adj = e :: nodes(n1).adj
    nodes(n2).adj = e :: nodes(n2).adj
  }

  override def edgeSymbol = "-"

  // TODO: Wrong implementation
  def spanningTrees1: List[Graph[T, U]] = {
    def spanR(sofar: Graph[T, U], currNode: Node): List[Graph[T, U]] = {
      // the new edge is not necessarily start from currNode 
      currNode.adj.filter(e => !sofar.nodes.contains(edgeTarget(e, currNode).get.value)).flatMap(e => {
        val t = edgeTarget(e, currNode).get.value
        val (soFarNodes, soFarEdges) = sofar.toTermForm
        val newG = Graph.termLabel(soFarNodes, soFarEdges)
        newG.addNode(t)
        newG.addEdge(e.toTuple._1, e.toTuple._2, e.toTuple._3)
        if (newG.hasCycles) Nil
        else {
          if (newG.nodes.size == Graph.this.nodes.size) List(newG)
          else spanR(newG, nodes(t))
        }
      })
    }
    nodes.keys.flatMap(n => spanR(Graph.termLabel(List(n), List.empty[(T, T, U)]), nodes(n))).toList
  }

  def edgeConnectsToGraph[T, U](e: Edge, nodes: List[Node]): Boolean =
    !(nodes.contains(e.n1) == nodes.contains(e.n2)) // xor
  def spanningTrees = {
    def spanningTreesR(graphEdges: List[Edge], graphNodes: List[Node], treeEdges: List[Edge]): List[Graph[T, U]] = {
      if (graphNodes == Nil) List(Graph.termLabel(nodes.keys.toList, treeEdges.map(_.toTuple)))
      else if (graphEdges == Nil) Nil
      else graphEdges.filter(edgeConnectsToGraph(_, graphNodes)) flatMap { ge =>
        spanningTreesR(graphEdges.remove(_ == ge),
          graphNodes.filter(edgeTarget(ge, _) == None),
          ge :: treeEdges)
      }
    }
    spanningTreesR(edges, nodes.values.toList.tail, Nil).removeDuplicates
  }
  def isTree: Boolean = spanningTrees.lengthCompare(1) == 0
  def isConnected: Boolean = spanningTrees.lengthCompare(0) > 0

  def minimalSpanningTree(implicit f: (U) => Ordered[U]): Graph[T, U] = {
    def spanningTreesR(graphEdges: List[Edge], graphNodes: List[Node], treeEdges: List[Edge]): Graph[T, U] = {
      if (graphNodes == Nil) Graph.termLabel(nodes.keys.toList, treeEdges.map(_.toTuple))
      else {
        val minimalEdge = graphEdges.filter(edgeConnectsToGraph(_, graphNodes)).reduceLeft((r, e) => if (r.value < e.value) r else e)
        spanningTreesR(graphEdges.remove(_ == minimalEdge),
          graphNodes.filter(edgeTarget(minimalEdge, _) == None),
          minimalEdge :: treeEdges)
      }
    }
    spanningTreesR(edges, nodes.values.toList.tail, Nil)
  }

  // My implementation
  def splitGraph1: List[Graph[T, U]] = {
    def connectedR(gn: List[Node], ge: List[Edge], cn: List[Node], ce: List[Edge]): List[Graph[T, U]] = {
      val me = ge.filter(edgeConnectsToGraph(_, cn))
      if (me == Nil) {
        val g = Graph.termLabel(cn.map(_.value), ce.map(_.toTuple))
        if (gn == Nil) List(g)
        else g :: connectedR(gn.tail, ge, List(gn.head), Nil)
      } else {
        val newNodes = me.foldLeft(List.empty[Node]) { (acc, e) =>
          if (!cn.contains(e.n1) && !acc.contains(e.n1)) e.n1 :: acc
          else e.n2 :: acc
        }
        connectedR(gn.filter(!newNodes.contains(_)), ge.filter(!me.contains(_)), newNodes ::: cn, me ::: ce)
      }
    }
    connectedR(nodes.values.toList.tail, edges, List(nodes.values.toList.head), Nil)
  }
}

class Digraph[T, U] extends GraphBase[T, U] {
  override def equals(o: Any) = o match {
    case g: Digraph[_, _] => super.equals(g)
    case _ => false
  }

  def edgeTarget(e: Edge, n: Node): Option[Node] =
    if (e.n1 == n) Some(e.n2)
    else None

  def addArc(source: T, dest: T, value: U) = {
    val e = new Edge(nodes(source), nodes(dest), value)
    edges = e :: edges
    nodes(source).adj = e :: nodes(source).adj
  }
  override def edgeSymbol = ">"
}

abstract class GraphObjBase {
  type GraphClass[T, U]
  def addLabel[T](edges: List[(T, T)]) =
    edges.map(v => (v._1, v._2, ()))
  def term[T](nodes: List[T], edges: List[(T, T)]) =
    termLabel(nodes, addLabel(edges))
  def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]): GraphClass[T, U]
  def addAdjacentLabel[T](nodes: List[(T, List[T])]) =
    nodes.map(a => (a._1, a._2.map((_, ()))))
  def adjacent[T](nodes: List[(T, List[T])]) =
    adjacentLabel(addAdjacentLabel(nodes))
  def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]): GraphClass[T, U]

  def fromString(s: String): GraphClass[String, Unit]
  def fromStringLabel(s: String): GraphClass[String, Int]
  def edgeAndLoneNodes(s: String, edgeSymbol: Char): (Array[(String, String)], Array[String]) = {
    val (edges, loneNodes) = splitEdgeAndLoneNodes(s, edgeSymbol)
    val edgesList = edges.map(e => {
      val parts = e.split(edgeSymbol)
      (parts(0), parts(1))
    })
    (edgesList, loneNodes)
  }
  def edgeAndLoneNodesLabel(s: String, edgeSymbol: Char): (Array[(String, String, String)], Array[String]) = {
    val (edges, loneNodes) = splitEdgeAndLoneNodes(s, edgeSymbol)
    val edgesList = edges.map(e => {
      val parts = e.split(Array(edgeSymbol, '/'))
      (parts(0), parts(1), parts(2))
    })
    (edgesList, loneNodes)
  }
  def splitEdgeAndLoneNodes(s: String, edgeSymbol: Char): (Array[String], Array[String]) =
    s.substring(1, s.length - 1).split(',').map(_.trim).partition(_.indexOf(edgeSymbol) != -1)
}

object Graph extends GraphObjBase {
  type GraphClass[T, U] = Graph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]) = {
    val g = new Graph[T, U]
    nodes.map(g.addNode)
    edges.map(v => g.addEdge(v._1, v._2, v._3))
    g
  }
  def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]) = {
    val g = new Graph[T, U]
    for ((v, a) <- nodes) g.addNode(v)
    for ((n1, a) <- nodes; (n2, l) <- a) {
      if (!g.nodes(n1).neighbors.contains(g.nodes(n2)))
        g.addEdge(n1, n2, l)
    }
    g
  }

  override def fromString(s: String): Graph[String, Unit] = {
    val (edges, loneNodes) = edgeAndLoneNodes(s, '-')
    val g = new Graph[String, Unit]
    edges.foreach(e => {
      g.addNode(e._1)
      g.addNode(e._2)
      g.addEdge(e._1, e._2, ())
    })
    loneNodes.foreach(g.addNode)
    g
  }

  override def fromStringLabel(s: String): Graph[String, Int] = {
    val (edges, loneNodes) = edgeAndLoneNodesLabel(s, '-')
    val g = new Graph[String, Int]
    edges.foreach(e => {
      g.addNode(e._1)
      g.addNode(e._2)
      g.addEdge(e._1, e._2, e._3.toInt)
    })
    loneNodes.foreach(g.addNode)
    g
  }
}

object Digraph extends GraphObjBase {
  type GraphClass[T, U] = Digraph[T, U]

  def termLabel[T, U](nodes: List[T], edges: List[(T, T, U)]) = {
    val g = new Digraph[T, U]
    nodes.map(g.addNode)
    edges.map(v => g.addArc(v._1, v._2, v._3))
    g
  }
  def adjacentLabel[T, U](nodes: List[(T, List[(T, U)])]) = {
    val g = new Digraph[T, U]
    for ((n, a) <- nodes) g.addNode(n)
    for ((s, a) <- nodes; (d, l) <- a) g.addArc(s, d, l)
    g
  }

  override def fromString(s: String): Digraph[String, Unit] = {
    val (edges, loneNodes) = edgeAndLoneNodes(s, '>')
    val g = new Digraph[String, Unit]
    edges.foreach(e => {
      g.addNode(e._1)
      g.addNode(e._2)
      g.addArc(e._1, e._2, ())
    })
    loneNodes.foreach(g.addNode)
    g
  }

  override def fromStringLabel(s: String): Digraph[String, Int] = {
    val (edges, loneNodes) = edgeAndLoneNodesLabel(s, '>')
    val g = new Digraph[String, Int]
    edges.foreach(e => {
      g.addNode(e._1)
      g.addNode(e._2)
      g.addArc(e._1, e._2, e._3.toInt)
    })
    loneNodes.foreach(g.addNode)
    g
  }

}

object TestGraph extends Application {
  //  println(Graph.term(List('b', 'c', 'd', 'f', 'g', 'h', 'k'),
  //    List(('b', 'c'), ('b', 'f'), ('c', 'f'), ('f', 'k'), ('g', 'h'))))
  //  println(Digraph.term(List('r', 's', 't', 'u', 'v'),
  //    List(('s', 'r'), ('s', 'u'), ('u', 'r'), ('u', 's'), ('v', 'u'))))
  //  println(Graph.fromString("[g-h,f-k,c-f,b-f,b-c,d]"))
  //  println(Graph.fromStringLabel("[g-h/3,f-k/2,c-f/7,b-f/12,b-c/22,d]"))
  //  println(Digraph.fromString("[s>r, t, u>r, s>u, u>s, v>u]"))
  //  println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]"))
  //
  //  println(Graph.fromString("[g-h,f-k,c-f,b-f,b-c,d]").toTermForm)
  //  println(Graph.fromStringLabel("[g-h/3,f-k/2,c-f/7,b-f/12,b-c/22,d]").toTermForm)
  //  println(Digraph.fromString("[s>r, t, u>r, s>u, u>s, v>u]").toTermForm)
  //  println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toTermForm)
  //
  //  println(Graph.fromString("[g-h,f-k,c-f,b-f,b-c,d]").toAdjacentForm)
  //  println(Graph.fromStringLabel("[g-h/3,f-k/2,c-f/7,b-f/12,b-c/22,d]").toAdjacentForm)
  //  println(Digraph.fromString("[s>r, t, u>r, s>u, u>s, v>u]").toAdjacentForm)
  //  println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm)
  //
  //  println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "q"))
  //  println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5]").findPaths("p", "k"))
  //
  //  println(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles("f"))
  //  println(Digraph.fromStringLabel("[p>q/9, m>q/7, k, p>m/5, q>s/10, s>p/5]").findCycles("p"))
  //
  //  println(Graph.fromString("[a-b, b-c, a-c]").spanningTrees)
  //  val st = Graph.term(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
  //    List(('a', 'b'), ('a', 'd'), ('b', 'c'), ('b', 'e'),
  //      ('c', 'e'), ('d', 'e'), ('d', 'f'), ('d', 'g'),
  //      ('e', 'h'), ('f', 'g'), ('g', 'h'))).spanningTrees
  //  println(st)
  //  println(st.size)
  //  val st1 = Graph.term(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
  //    List(('a', 'b'), ('a', 'd'), ('b', 'c'), ('b', 'e'),
  //      ('c', 'e'), ('d', 'e'), ('d', 'f'), ('d', 'g'),
  //      ('e', 'h'), ('f', 'g'), ('g', 'h'))).spanningTrees1
  //  println(st -- st1)
  //  //  val g1 = Graph.fromString("[a-c,b-c]")
  //  //  val g2 = Graph.fromString("[b-c,a-c]")
  //  //  println(g1 == g2)
  //  //  println(g1.hashCode())
  //  //  println(g2.hashCode())
  //  
  //  println(Graph.fromStringLabel("[a-b/1, b-c/2, a-c/3]").minimalSpanningTree)
  //  println(Graph.termLabel(
  //  List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
  //       List(('a', 'b', 5), ('a', 'd', 3), ('b', 'c', 2), ('b', 'e', 4),
  //            ('c', 'e', 6), ('d', 'e', 7), ('d', 'f', 4), ('d', 'g', 3),
  //            ('e', 'h', 5), ('f', 'g', 4), ('g', 'h', 1))).minimalSpanningTree)

  val g1 = Graph.fromString("[a-b,b-c,c-f]")
  val g2 = Graph.fromString("[1-2,1-3,1-4]")
  //  println(g1.bijection(g2))
  println(g1.isIsomorphicTo(g2))

  println(Graph.fromString("[a-b, b-c, a-c, a-d]").nodes("a").degree)
  println(Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByDegree)
  println(Graph.fromString("[a-b, b-c, a-c, a-d]").colorNodes)

  println(Graph.fromString("[a-b, b-c, e, a-c, a-d, a-f, f-b, g-d]").nodesByDepthFrom("d"))

  println(Graph.fromString("[a-b, b-d, d-g, a-h, c-e]").splitGraph)

  println(Digraph.fromString("[a>b, c>a, d>b]").isBipartite)
  println(Graph.fromString("[a-b, b-c, c-a]").isBipartite)
  println(Graph.fromString("[a-b, b-c, d]").isBipartite)
  println(Graph.fromString("[a-b, b-c, d, e-f, f-g, g-e, h]").isBipartite)
}


