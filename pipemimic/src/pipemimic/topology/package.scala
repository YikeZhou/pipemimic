package pipemimic

import scala.collection.mutable.ListBuffer

package object topology {

  type Node = Int

  type AdjacencyList = List[List[Node]]

  type Edge = (Node, Node)
  type EdgeList = List[Edge]

  implicit class AdjacencyListFromEdges(val edgeList: EdgeList) {
    def toAdjacencyList: AdjacencyList = {
      val nodeCnt = edgeList.foldLeft(0){ case (max, (u, v)) => math.max(max, math.max(u, v)) } + 1

      val adj = List.fill(nodeCnt)(ListBuffer.empty[Int])
      edgeList foreach { case (u, v) =>
        adj(u) += v
      }
      adj.map(_.toList)
    }
  }

  sealed abstract class TopSortResult

  case class TotalOrdering(order: List[Node]) extends TopSortResult
  case class CycleFound(cycle: List[Node]) extends TopSortResult

  object Marking extends Enumeration {
    val Unmarked, MarkedTemp, Marked = Value
  }

  implicit class VerifyMustHappenBeforeInGraph(g: List[(Int, Int, String)]) {
    def existsPath(src: Int, dst: Int): MHBResult = {
      val graphWithoutLabel = g map { case (s, d, _) => (s, d) }
      TopologicalSort(graphWithoutLabel) match {
        case TotalOrdering(_) =>
          /* find path from src to dst */
          val path = PathFinder.findPath(graphWithoutLabel.toAdjacencyList, src, dst)
          if (path.nonEmpty) MustHappenBefore(g, path) else Unverified(g, src, dst)
        case CycleFound(cycle) =>
          Cyclic(g, cycle)
      }
    }
    def existsPath(sd: (Int, Int)): MHBResult = existsPath(sd._1, sd._2)
  }

}
