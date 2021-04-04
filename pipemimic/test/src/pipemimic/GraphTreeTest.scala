package pipemimic

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import GraphTree._

class GraphTreeTest extends AnyFlatSpec {
  "GraphTree" should "calculate DNFOfTree correctly" in {
    val gt1 = GraphTreeAnd(List(
      GraphTreeLeaf("A", List((1, 2, "a"))),
      GraphTreeLeaf("B", List((3, 4, "b")))
    ))
    val gt2 = GraphTreeAnd(List(
      GraphTreeLeaf("A", List((1, 2, "a"))),
      GraphTreeOr(List(
        GraphTreeLeaf("B", List((3, 4, "b"))),
        GraphTreeLeaf("C", List((5, 6, "c")))
      ))
    ))
    val gt3 = GraphTreeAnd(List(
      GraphTreeLeaf("A", List((1, 2, "a"))),
      GraphTreeLeaf("B", List((7, 8, "d"))),
      GraphTreeOr(List(
        GraphTreeLeaf("C", List((3, 4, "b"))),
        GraphTreeLeaf("D", List((5, 6, "c")))
      ))
    ))
    val gt4 = GraphTreeAnd(List(
      GraphTreeLeaf("A", List((1, 2, "a"))),
      GraphTreeAnd(List(
        GraphTreeEmptyLeaf[Int],
        GraphTreeLeaf("B", List((3, 4, "b")))
      ))
    ))
    val gt5 = GraphTreeAnd(List(
      GraphTreeLeaf("A", List((1, 2, "a"))),
      GraphTreeOr(List.empty[GraphTree[Int]])
    ))

    val trees = Seq(gt1, gt2, gt3, gt4, gt5)

    def print_node(i: Int): String = i.toString

    assert(DNFOfTree(gt1) == List(("AB", List((1, 2, "a"), (3, 4, "b")))))
    assert(DNFOfTree(gt2) == List(("AB", List((1, 2, "a"), (3, 4, "b"))), ("AC", List((1, 2, "a"), (5, 6, "c")))))
    assert(DNFOfTree(gt3) == List(("ABC", List((1, 2, "a"), (7, 8, "d"), (3, 4, "b"))), ("ABD", List((1, 2, "a"), (7, 8, "d"), (5, 6, "c")))))
    assert(DNFOfTree(gt4) == List(("AB", List((1, 2, "a"), (3, 4, "b")))))
    assert(DNFOfTree(gt5) == Nil)

    for (i <- 0 to 4) println(DNFStringOfTree(print_node, trees(i)))
  }
}
