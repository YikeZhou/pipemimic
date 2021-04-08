package pipemimic.topology

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

class TopologicalSortTest extends AnyFlatSpec {
  "TopologicalSort" should "detect cycle in graph" in {
    var in = List((0, 3), (2, 0), (2, 3), (3, 1))
    var out: TopSortResult = TotalOrdering(List(2, 0, 3, 1))
    assert(TopologicalSort(in) == out)

    in = List((0, 1), (1, 0))
    out = CycleFound(List(0, 1, 0))
    assert(TopologicalSort(in) == out)

    in = List((0, 1), (1, 2), (2, 3), (3, 5), (5, 1))
    assert(TopologicalSort(in).isInstanceOf[CycleFound])
    println(TopologicalSort(in))
  }
}
