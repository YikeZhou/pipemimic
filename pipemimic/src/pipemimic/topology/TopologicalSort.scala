package pipemimic.topology

import scala.collection.mutable

class TopologicalSort(g: EdgeList) {

  private val adj: AdjacencyList = g.toAdjacencyList
  private val nodeCnt = adj.length

  private var result: TopSortResult = _

  /* Given an adjacency list, return a list of incoming degree for each node */
  private val incomingDegree = {
    val degrees = Array.fill(nodeCnt)(0)
    adj.foreach { neighbors =>
      for (v <- neighbors)
        degrees(v) += 1
    }
    degrees
  }

  /* Kahn Algorithm */
  private val nodesWithNoIncomingEdges = mutable.Queue.empty[Node]
  nodesWithNoIncomingEdges.enqueueAll((0 until nodeCnt).filter(incomingDegree(_) == 0))

  private val order = mutable.Queue.empty[Node]

  while (nodesWithNoIncomingEdges.nonEmpty) {
    val u = nodesWithNoIncomingEdges.dequeue()
    order.enqueue(u)
    for (v <- adj(u)) {
      incomingDegree(v) -= 1
      if (incomingDegree(v) == 0)
        nodesWithNoIncomingEdges.enqueue(v)
    }
  }

  if (order.size == nodeCnt) {
    result = TotalOrdering(order.toList)
  } else {
    val n = incomingDegree.indexWhere(_ != 0)
    result = CycleFound(PathFinder.findPath(adj, n, n))
  }
}

object TopologicalSort {
  def apply(g: EdgeList): TopSortResult = {
    val topologicalSort = new TopologicalSort(g)
    topologicalSort.result
  }
}
