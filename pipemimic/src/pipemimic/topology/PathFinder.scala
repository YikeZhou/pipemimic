package pipemimic.topology

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

class PathFinder(g: AdjacencyList, src: Node) {
  private val nodeCnt = g.length
  private var cyclic = false
  /* find reachable nodes starting at src node */
  private var maxIteration = g.map(_.length).sum + g.length

  private val queue = mutable.Queue.empty[Node]
  queue.enqueue(src)
  /* save reachable nodes from src */
  private val allReachableNodesFromSrc = mutable.Set.empty[Node]
  /* prev nodes */
  private val prev: Array[Option[Node]] = Array.fill(nodeCnt)(None)

  /* Dijkstra algorithm */
  breakable {
    while (queue.nonEmpty) {
      if (maxIteration == 0) {
        /* which should not happen */
        sys.error("dijkstra exceeding max iteration\n")
        break
      }

      val u = queue.dequeue()
      /* execute one dijkstra step */
      val adjacentNodes = g(u)
      if (adjacentNodes.contains(src)) {
        /* find a cycle in graph g, stop here */
        cyclic = true
        if (prev(src).isEmpty)
          prev(src) = Some(u)
        allReachableNodesFromSrc.clear()
        break
      } else {
        /* add adjacent node into queue */
        for (v <- adjacentNodes if !queue.contains(v))
          queue.enqueue(v)
        /* update reachable list */
        allReachableNodesFromSrc.addAll(adjacentNodes)
        allReachableNodesFromSrc.addOne(u)
        /* update prev list */
        for (v <- adjacentNodes if prev(v).isEmpty)
          prev(v) = Some(u)
      }

      maxIteration -= 1
    }
  }

  def isReachable(n: Node): Boolean = allReachableNodesFromSrc.contains(n)

  def getAllReachableNodes: List[Node] = allReachableNodesFromSrc.toList

  def getPrev(n: Node): Option[Node] = prev(n)

  def findPath(dst: Node): List[Node] = {
    var maxIter = nodeCnt
    val path = mutable.Stack.empty[Node]

    var curNode = dst
    path.push(dst)

    breakable {
      while (prev(curNode).isDefined) {
        if (maxIter == 0)
          break

        curNode = prev(curNode).get
        path.push(curNode)
        if (curNode == src)
          break
        maxIter -= 1
      }
    }

    if (path.headOption.isEmpty || path.headOption.get != src)
      Nil /* no path from src to dst */
    else
      path.toList /* return list of nodes from src to dst */
  }
}

object PathFinder {
  def apply(g: EdgeList, src: Node): PathFinder = new PathFinder(g.toAdjacencyList, src)

  def findPath(g: AdjacencyList, src: Node, dst: Node): List[Node] = {
    val pathFinder = new PathFinder(g, src)
    pathFinder.findPath(dst)
  }
}
