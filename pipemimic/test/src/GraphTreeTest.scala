package pipemimic

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import GraphTree._

class GraphTreeTest extends AnyFlatSpec {
  behavior of "GraphTree"

  it should "calculate DNFOfTree correctly" in {
    val gt1 = GraphTreeAnd(List(GraphTreeLeaf("A", List((1, 2, "a"))), GraphTreeLeaf("B", List((3, 4, "b")))))
    assert(DNFOfTree(gt1) == List(("AB", List((1, 2, "a"), (3, 4, "b")))))
  }
}
