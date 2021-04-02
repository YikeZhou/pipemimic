package pipemimic

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

class TinyTimer(name: String) {
  var start: Long = _
  var init = false

  def reset(): Unit = {
    init = true
    start = System.nanoTime()
  }

  override def toString: String = {
    val timeElapsed = (System.nanoTime() - start) / 1000000
    if (init) s"Timer<$name>: $timeElapsed ms" else "Error: not initialized"
  }
}

object GlobalGraphIDUtils {
  def geid(p: Pipeline, ge: GlobalEvent): Int = {
    ge match {
      case (n, e) => e * p.stages.length + n
    }
  }

  def gepid(p: Pipeline, gep: (GlobalEvent, GlobalEvent, String)): (Int, Int, String) = {
    (geid(p, gep._1), geid(p, gep._2), gep._3)
  }

  def getid(p: Pipeline, t: GraphTree[GlobalEvent]): GraphTree[Int] = { // TODO: Pipeline => pipeline.stages.length
    t match {
      case GraphTreeOr(l) => GraphTreeOr(l.map(getid(p, _)))
      case GraphTreeAnd(l) => GraphTreeAnd(l.map(getid(p, _)))
      case GraphTreeLeaf(s, l) => GraphTreeLeaf(s, l.map(gepid(p, _)))
    }
  }

  def ungeid(p: Pipeline, n: Int): (Stages.Location, ProgramOrderIndex) = {
    @tailrec
    def helper(p: Pipeline, n: Int, s: Int, e: Int): (Stages.Location, ProgramOrderIndex) = {
      if (s == p.stages.length) {
        if (n == 0) (0, e + 1) else helper(p, n - 1, 1, e + 1)
      } else if (s < p.stages.length) {
        if (n == 0) (s, e) else helper(p, n - 1, s + 1, e)
      } else {
        /* ERROR */ (0, 0)
      }
    }
    
    helper(p, n, 0, 0)
  }
}

object Dot {
  private val startColor = "#d6fced"
  private val middleColor = "#8dfbe4"
  private val finishColor = "#83e0d8"

  val edgeColor: Map[String, String] = HashMap(
    "ProgramOrder" -> "#ff0000", // Blue
    "IntraLocation" -> "#00cc00", // Green
    "IntraEvent" -> "#000000", // Black
    "RF" -> "#ff0000", // Bright red
    "WS" -> "#ff0000", // Med red
    "FR" -> "#ff0000", // Dark red
    "Other" -> "#0000ff" // Yellow
  )

  def EdgeColor(l1: Int, l2: Int, edgeString: String): String = {
    val color = if (l1 == 0 && l2 == 0) edgeColor("ProgramOrder") 
      else edgeColor.getOrElse(edgeString, edgeColor("Other"))
    s"""color="$color""""
  }

  def EdgeDotted(n1: Int, n2: Int, lThick: List[(Int, Int)]): String = {
    lThick match {
      case Nil => ""
      case _ => if (lThick.contains((n1, n2))) "" else "style=dashed"
    }
  }

  def FormatString(l: List[String]): String = {
    l.filter(!_.isEmpty) match {
      case Nil => ""
      case x => x.mkString("[", ",", "]")
    }
  }

  def natPairToString(fString: Int => String, fStageEvent: Int => (Int, Int), lThick: List[(Int, Int)], np: (Int, Int, String)): String = {
    val (n1, n2) = (np._1, np._2)
    val (l1, e1) = fStageEvent(n1)
    val (l2, e2) = fStageEvent(n2)
    val formatString = FormatString(
      EdgeDotted(n1, n2, lThick) ::
      EdgeColor(l1, l2, np._3) ::
      { if (lThick.contains((n1, n2))) "penwidth=5" else "" } ::
      { if (e1 == e2) "" else "constraint=false" } :: Nil
    )
    s"  ${fString(n1)} -> ${fString(n2)}$formatString; // ($n1, $n2) ${np._3}\n"
  }

  def GroupNodesByEvent(f: Int => (Int, Int), l: List[Int]): List[List[Int]] = {
    l match {
      case head :: next =>
        val (_, e) = f(head)
        GroupNodesByEvent(f, next).appendToNth(e, head, isUnique = true)
      case Nil => Nil
    }
  }

  @tailrec
  def EdgeListToNodeList(l: List[(Int, Int, String)], f: Int => (Int, Int), rNodes: List[Int]): List[Int] = {
    l match {
      case head :: next =>
        val (h1, h2) = (head._1, head._2)
        val _rNodes = rNodes.addUnique(h1).addUnique(h2)
        EdgeListToNodeList(next, f, _rNodes)
      case Nil => rNodes
    }
  }

  def SubgraphClusterEntry(nodeToString: Int => String, lBold: List[Int], nodeNumber: Int): String = {
    val format = {
      if (lBold.isEmpty) ""
      else if (nodeNumber == lBold.head) s""" [style=filled,color="$startColor"]"""
      else if (nodeNumber == lBold.last) s""" [style=filled,color="$finishColor"]"""
      else if (lBold.contains(nodeNumber)) s""" [style=filled,color="$middleColor"]"""
      else ""
    }
    s"    ${nodeToString(nodeNumber)}$format; // $nodeNumber\n"
  }

  def SubgraphCluster(eventNumber: Int, nodeToString: Int => String, lBold: List[Int], listOfNodesForEvent: List[Int]): String = {
    s"""  subgraph cluster_$eventNumber  {
        style=filled;
        color=white;
        label="Event$eventNumber"
        ${listOfNodesForEvent.map(SubgraphClusterEntry(nodeToString, lBold, _)).mkString}  }
    """
  }

  def SubgraphClusters(nodeToString: Int => String, lBold: List[Int], listOfNodesForEachEvent: List[List[Int]])
  : String = {
    def helper(eventNumber: Int, nodeToString: Int => String, lBold: List[Int],
               listOfNodesForEachEvent: List[List[Int]]): String = {
      listOfNodesForEachEvent match {
        case head :: next => SubgraphCluster(eventNumber, nodeToString, lBold, head) ++ helper(eventNumber + 1,
          nodeToString, lBold, next)
        case Nil => ""
      }
    }
    
    helper(0, nodeToString, lBold, listOfNodesForEachEvent)
  }

  def rankNodes(f: Int => Int, pMax: Int, l: List[Int]): (List[List[Int]], List[Int]) = {
    @tailrec
    def helper(f: Int => Int, pMax: Int, l: List[Int], r1: List[List[Int]], r2: List[Int]): (List[List[Int]], List[Int]) = {
      l match {
        case head :: next => if (pMax <= f(head)) helper(f, pMax, next, r1, r2.addUnique(head)) else
          helper(f, pMax, next, r1.appendToNth(f(head), head), r2)
        case Nil => (r1, r2)
      }
    }
    
    helper(f, pMax, l, Nil, Nil)
  }

  def standaloneNode(f: Int => String, lBold: List[Int], n: Int): String = {
    val format = {
      if (lBold.isEmpty) ""
      else if (n == lBold.head) s""" [style=filled,color="$startColor"]"""
      else if (n == lBold.last) s""" [style=filled,color="$finishColor"]"""
      else if (lBold.contains(n)) s""" [style=filled,color="$middleColor"]"""
      else ""
    }
    s"    ${f(n)}$format; // node $n\n"
  }

  def rank(fString: Int => String, lBold: List[Int], l: List[Int]): String = {
    s"  {rank=same;\n${l.map(standaloneNode(fString, lBold, _)).mkString}  }\n"
  }

  def ranks(fString: Int => String, lBold: List[Int], l: List[List[Int]]): String = {
    l.map(rank(fString, lBold, _)).mkString
  }

  def unranked(fString: Int => String, lBold: List[Int], l: List[Int]): String = {
    l.map(standaloneNode(fString, lBold, _)).mkString
  }

  def EdgeStrings(fString: Int => String, fStageEvent: Int => (Int, Int), l: List[(Int, Int, String)], lThick: List[(Int, Int)]): String = {
    l.map(natPairToString(fString, fStageEvent, lThick, _)).mkString
  }

  /**
    * Build a dot graph from the description of the graph, plus some formatting options.
    *
    * @param name the title of the graph
    * @param lNormal the set of edges in the graph
    * @param fStageEvent given a vertex, return the location and event the vertex represents
    * @param fString given a vertex, return a label for the vertex
    * @param lBold a list of nodes that should be bolded/filled in
    * @param lThick a list of edges that should be colored and thickened
    * @param pMax the length of the pipeline. Vertices with locations > pMax (i.e., cache events) will noe necessarily be clustered with the pipeline.
    * @return graph described in dot grammar
    */
  def DotGraph(name: String, lNormal: List[(Int, Int, String)], fStageEvent: Int => (Int, Int), fString: Int => String,
               lBold: List[Int], lThick: List[(Int, Int)], pMax: Int): String = {
    val ms = new TinyTimer("dot_graph")
    ms.reset()
    val fStage: Int => Int = fStageEvent(_)._1
    val fEvent: Int => Int = fStageEvent(_)._2
    val lNodes = EdgeListToNodeList(lNormal, fStageEvent, Nil)
    val lSameEvent = GroupNodesByEvent(fStageEvent, lNodes)
    val (lRanked, lUnranked) = rankNodes(fStage, pMax, lNodes)
    val result = s"""// nodes per column: $pMax
    digraph PipeCheck {
      labellloc="t";
      label="$name";
      newrank=true;
      ${SubgraphClusters(fString, lBold, lSameEvent)}${ranks(fString, lBold, lRanked)}
      ${unranked(fString, lBold, lUnranked)}${EdgeStrings(fString, fStageEvent, lNormal, lThick)}}
      """
    println(ms)
    result
  }
}