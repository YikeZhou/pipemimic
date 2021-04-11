package pipemimic.topology

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

class PathFinderTest extends AnyFlatSpec {

  behavior of "PathFinder"

  private val exampleGraph = (0, 3) :: (2, 0) :: (2, 3) :: (3, 1) :: Nil

  it should "yield correct path in given graph" in {
    assert(PathFinder.findPath(exampleGraph.toAdjacencyList, 2, 3) == List(2, 3))
    assert(PathFinder.findPath(exampleGraph.toAdjacencyList, 2, 1) == List(2, 3, 1))
  }

  it should "given correct reachable vertices" in {
    val pathFinder2 = PathFinder(exampleGraph, 2)
    assert(pathFinder2.getAllReachableNodes.contains(0))
    assert(pathFinder2.getAllReachableNodes.contains(3))
    assert(pathFinder2.getAllReachableNodes.contains(1))

    val pathFinder0 = PathFinder(exampleGraph, 0)
    assert(!pathFinder0.getAllReachableNodes.contains(2))
    assert(pathFinder0.getAllReachableNodes.contains(3))
    assert(pathFinder0.getAllReachableNodes.contains(1))
  }
}
