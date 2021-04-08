package pipemimic

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import Stages.{GlobalEvent, GlobalGraph, GlobalEventString, GraphString, GlobalGraphString}

class GraphStringTest extends AnyFlatSpec {
  behavior of "GraphString functions"
  it should "print graph correctly" in {
    val exampleGraph = List(
      (0, 1, "a"),
      (0, 2, "b")
    )

    val graphString = "0 --b-> 2\n0 --a-> 1\n"

    assert(GraphString(exampleGraph) == graphString)

    val exampleGlobalGraph = List(
      ((0, 0), (0, 1), "a"),
      ((0, 0), (1, 0), "b")
    )

    val globalGraphString = "(0, 0) --b-> (1, 0)\n(0, 0) --a-> (0, 1)\n"

    assert(GlobalGraphString(exampleGlobalGraph) == globalGraphString)
  }
}
