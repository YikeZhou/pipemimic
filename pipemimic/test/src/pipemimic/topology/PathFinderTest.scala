package pipemimic.topology

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

class PathFinderTest extends AnyFlatSpec {
  "PathFinder" should "yield correct path in given graph" in {
    val exampleGraph = (0, 3) :: (2, 0) :: (2, 3) :: (3, 1) :: Nil
    val result = List(2, 3)

    assert(PathFinder.findPath(exampleGraph.toAdjacencyList, 2, 3) == result)
  }
}
