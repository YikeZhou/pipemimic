package pipemimic

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import Adjacency._

class DijkstraTest extends AnyFlatSpec {
  "AdjacencyListFromEdges" should "return adjacency list" in {
    var exampleGraph = (0, 1) :: (0, 2) :: (1, 3) :: (2, 3) :: Nil
    var adjacencyList = List(2, 1) :: List(3) :: List(3) :: Nil

    assert(AdjacencyListFromEdges(exampleGraph) == adjacencyList)
  }

  "DijkstraAdj" should "be alright" in {
    val exampleGraph = List(3) :: List() :: List(0, 3) :: List(1) :: Nil
    var result = (List(0, 3, 1), List(Some(2), Some(3), None, Some(2)))

    assert(DijkstraAdj(exampleGraph, 2) == result)
    
    result = (List(3, 1), List(None, Some(3), None, Some(0)))

    assert(DijkstraAdj(exampleGraph, 0) == result)
  }

  "Dijkstra" should "calculate the set of vertices reachable from [src]" in {
    var exampleGraph = (0, 3) :: (2, 0) :: (2, 3) :: (3, 1) :: Nil
    var result = (List(3, 1), None :: Some(3) :: None :: Some(0) :: Nil)
    
    assert(Dijkstra(exampleGraph, 0) == result)

    exampleGraph = (0, 1) :: (1, 0) :: Nil
    result = (List(0, 1), List(Some(1), Some(0)))

    assert(Dijkstra(exampleGraph, 0) == result)

    exampleGraph = (0, 1) :: (0, 2) :: (1, 3) :: (2, 3) :: Nil
    result = (List(2, 1, 3), List(None, Some(0), Some(0), Some(2)))

    assert(Dijkstra(exampleGraph, 0) == result)
  }

  "FindPath" should "be alright too" in {
    val exampleGraph = (0, 3) :: (2, 0) :: (2, 3) :: (3, 1) :: Nil
    val result = List(2, 3)
    assert(FindPath(exampleGraph, 2, 3) == List(2, 3))

    assert(PathBacktraceAdj(List(List(1)), List(None, Some(0)), 0, 1) == List(0, 1))

    assert(FindPath(List((0, 1), (0, 2), (1, 2), (2, 3), (3, 0)), 0, 0) == List(0, 2, 3, 0))

    assert(FindPath(List((0, 1), (0, 2)), 1, 2).isEmpty)
  }
}