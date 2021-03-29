package pipemimic

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import Stages._
import GraphTree._
import RISCTest.RISCPipeline
import Execution.ScenariosForEvents
import pipemimic.GlobalGraphIDUtils.{ungeid, getid}
import pipemimic.PreservedProgramOrder.AllScenariosForPOWithAnyAddress
import java.io._

class StaticGraphTest extends AnyFlatSpec {

  "ScenarioEdges" should "generate proper static edges" in {
    val pipeline = RISCPipeline(1)
    val scenarios = AllScenariosForPOWithAnyAddress(pipeline, List(Direction.R, Direction.R))

    /* test scenario 1 first */
    val s = scenarios.head
    println(s"Scenario Title: ${s._1}")
    val graph = ScenarioEdges("PPO", pipeline, s._2) /* static edges ? */
    println(graph)
    val raw = getid(pipeline, graph)
    val g = DNFOfTree(raw)
    assert(g.length == 1)
    val finalGraph = g.head
    val dot = Dot.DotGraph("Test case 1", finalGraph._2, ungeid(pipeline, _), x => x.toString, Nil, Nil, pipeline.stages.length)

    /* print to files */
    println(dot)
    val writer = new PrintWriter(new File("graphs/StaticGraph.gv"))
    writer.write(dot)
    writer.close()
  }
}
