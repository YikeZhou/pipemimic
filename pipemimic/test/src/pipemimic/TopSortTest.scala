package pipemimic

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

class TopSortTest extends AnyFlatSpec with AcyclicCheck {
  "EdgesWithConnections" should "return boolean list" in {
    val input = List(List(12, 8, 4), Nil, Nil, Nil, List(12, 8), Nil, Nil, Nil, List(12))
    assert(EdgesWithConnections(input) == List(true, false, false, false, true, false, false, false, true, false, false, false, true))
  }

  "TopSortAdj" should "work well" in {
    val in = List(List(3), Nil, List(0, 3), List(1))
    val result = TotalOrdering(List(2, 0, 3, 1))
    assert(TopSortAdj(in) == result)
  }

  "TopSort" should "work well too" in {
    var in = List((0, 3), (2, 0), (2, 3), (3, 1))
    var out: TopSortResult = TotalOrdering(List(2, 0, 3, 1))
    assert(TopSort(in) == out)
    
    in = List((0, 1), (1, 0))
    out = CycleFound(List(1, 0, 1))
    assert(TopSort(in) == out)
  }
}
