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

}
