package pipemimic

import scala.collection.mutable
import java.io._

/**
  * Build a dot graph
  * @param name the title of the graph
  * @param normalLines set of edges
  * @param extractor given a vertex, return the location and event the vertex represents
  * @param nodeToString given a vertex, return a label for the vertex
  * @param boldNodes a list of nodes that should be bolded/filled in
  * @param thickLines a list of edges that should be colored and thickened
  * @param maxNodesPerColumn the length of the pipeline. Vertices with locations > pMax (i.e., cache events)
  *                          will not necessarily be clustered with the pipeline.
  */
class DotGraph(name: String, normalLines: List[(Int, Int, String)], extractor: Int => (Int, Int), nodeToString: Int
  => String, boldNodes: List[Int], thickLines: List[(Int, Int)], maxNodesPerColumn: Int) {
  private val gv = new StringBuilder

  private def getStage: Int => Int = extractor(_)._1
  private def getEvent: Int => Int = extractor(_)._2

  private def getNodeList: mutable.Set[Int] = {
    val nodes = mutable.Set.empty[Int]
    normalLines foreach { case (s, d, _) =>
      nodes += s
      nodes += d
    }
    nodes
  }

  private val allNodes = getNodeList

  private val nodesGroupByEvent: mutable.Map[Int, mutable.Set[Int]] = {
    val nodesByEvent = mutable.Map.empty[Int, mutable.Set[Int]]
    allNodes foreach { n =>
      val e = getEvent(n)
      if (nodesByEvent.contains(e))
        nodesByEvent(e) += n
      else
        nodesByEvent += (e -> mutable.Set(n))
    }
    nodesByEvent
  }

  private def rankNodes: (mutable.Map[ProgramOrderIndex, mutable.Set[ProgramOrderIndex]], mutable.Set[ProgramOrderIndex])
  = {
    val ranked = mutable.Map.empty[Int, mutable.Set[Int]]
    val unranked = mutable.Set.empty[Int]
    allNodes foreach { n =>
      val s = getStage(n)
      if (maxNodesPerColumn <= getStage(n))
        unranked += n
      else if (ranked.contains(s))
        ranked(s) += n
      else
        ranked += (s -> mutable.Set(n))
    }
    (ranked, unranked)
  }

  private val (rankedNodes, unrankedNodes) = rankNodes

  private val startColor = "#d6fced"
  private val middleColor = "#8dfbe4"
  private val finishColor = "#83e0d8"

  private def edgeFormat(nodeNumber: Int, l: List[Int]): String = {
    if (l.isEmpty) ""
    else if (nodeNumber == l.head) s"""[style=filled,color="$startColor"]"""
    else if (nodeNumber == l.last) s"""[style=filled,color="$finishColor"]"""
    else if (l.contains(nodeNumber)) s"""[style=filled,color="$middleColor"]"""
    else ""
  }

  private def subgraphClusters: String = {

    def subgraphClusterEntry(nodeNumber: Int): String = {
      val format = edgeFormat(nodeNumber, boldNodes)
      s"${nodeToString(nodeNumber)} $format; // $nodeNumber"
    }

    def subgraphCluster(eventNumber: Int): String = {
      val events = nodesGroupByEvent.getOrElse(eventNumber, mutable.Set.empty)
      assert(events.nonEmpty)
      s"""  subgraph cluster_event$eventNumber {
         |  style=filled;
         |  color=white;
         |  label="Event$eventNumber";
         |  ${events.map(subgraphClusterEntry).mkString("\n  ")}
         |  }
         |""".stripMargin
    }

    (0 until nodesGroupByEvent.keySet.size).map(subgraphCluster).mkString
  }

  private def standaloneNode(n: Int): String = {
    val format = edgeFormat(n, boldNodes)
    s"${nodeToString(n)}$format; // node $n"
  }

  private def ranks: String = {
    def rank(l: mutable.Iterable[Int]): String = {
      s"""  {
         |    rank=same;
         |    ${l.map(standaloneNode).mkString("\n    ")}
         |  }
         |""".stripMargin
    }
    rankedNodes.values.map(rank).mkString
  }

  private def unranked: String = {
    unrankedNodes.map(standaloneNode).mkString("\n")
  }

  private def edges: String = {

    val edgeColors = Map(
      "ProgramOrder" -> "#ff0000", // Blue
      "IntraLocation" -> "#00cc00", // Green
      "IntraEvent" -> "#000000", // Black
      "RF" -> "#ff0000", // Bright red
      "WS" -> "#ff0000", // Med red
      "FR" -> "#ff0000", // Dark red
      "Other" -> "#0000ff" // Yellow
    )

    def edgeColor(n1: Int, n2: Int, label: String): String = {
      val color = // FIXME does program order always lays in stage 0 ?
        if (n1 == 0 && n2 == 0) edgeColors("ProgramOrder") else edgeColors.getOrElse(label, edgeColors("Other"))
      s"""color="$color""""
    }

    def intPair2String(node: (Int, Int, String)): String = {
      val (n1, n2, label) = node
      val (stage1, event1) = extractor(n1)
      val (stage2, event2) = extractor(n2)

      def formatString(l: Seq[String]): String = {
        l.filter(_.nonEmpty) match {
          case Nil => ""
          case x => x.mkString("[", ",", "]")
        }
      }
      val attrs = List(
        edgeColor(stage1, stage2, label),
        if (thickLines.contains((n1, n2))) "" else "style=dashed",
        if (thickLines.contains((n1, n2))) "penwidth=5" else "",
        if (event1 == event2) "" else "constraint=false"
      )
      s"  ${nodeToString(n1)} -> ${nodeToString(n2)}${formatString(attrs)}; // ($n1, $n2) $label\n"
    }
    normalLines.map(intPair2String).mkString
  }

  /**
    * Graph string in graphviz format
    */
  val formattedGraph: String = {
    gv.append(s"// nodes per column: $maxNodesPerColumn\n")
      .append("digraph PipeCheck {\n")
      .append("""  labelloc="t";""").append('\n')
      .append(s"""  label="$name";""").append('\n')
      .append("  newrank=true;\n\n")
      .append(subgraphClusters)
      .append(ranks)
      .append(unranked)
      .append(edges)
      .append('}')
    gv.toString()
  }

  /**
    * Write graphviz file into given path
    * @param path path to save the file (shouldn't have / at the end)
    */
  def write(path: String): Unit = {
    require(path.nonEmpty && path.last != '/')
    val writer = new PrintWriter(new File(s"$path/$name.gv"))
    writer.write(formattedGraph)
    writer.close()
  }
}
