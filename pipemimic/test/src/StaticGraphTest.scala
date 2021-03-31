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
  def StaticGraph(p: Pipeline, po: List[Direction.Value]): List[String] = {
    val scenarios = AllScenariosForPOWithAnyAddress(p, po)
    val graphs = scenarios map {
      s => 
        println("Scenario Title: " + s._1)
        val se = ScenarioEdges("PPO", p, s._2)
        println(se)
        val raw = getid(p, se)
        val gs = raw.flatten
        println(s"found ${gs.length} trees")
        val dots: List[String] = gs.zipWithIndex map {
          case (g, i) => Dot.DotGraph(s"Case $i: ${g._1}", g._2, ungeid(p, _), _.toString, Nil, Nil, p.stages.length)
        }
        dots
    }
    graphs.flatten
  }

  "ScenarioEdges" should "generate proper static edges" in {
    val pipeline = RISCPipeline(4)
    val scenarios = AllScenariosForPOWithAnyAddress(pipeline, List(Direction.W, Direction.W, Direction.R))

    /* test scenario 1 first */
    val s = scenarios.head
    println(s"Scenario Title: ${s._1}")
    val graph = ScenarioEdges("PPO", pipeline, s._2) /* static edges ? */
    println(graph)
    val raw = getid(pipeline, graph)
    val g = raw.flatten
    assert(g.length == 1)
    val finalGraph = g.head
    val dot = Dot.DotGraph("Test case 1", finalGraph._2, ungeid(pipeline, _), 
      x => x.toString, Nil, Nil, pipeline.stages.length)

    /* print to files */
    println(dot)
    val writer = new PrintWriter(new File("graphs/StaticGraph.gv"))
    writer.write(dot)
    writer.close()
    
    /* all graphs in risc */
    val dots = StaticGraph(pipeline, List(Direction.R, Direction.R))
    
    dots.zipWithIndex foreach {
      case (d, i) =>
        println(d)
        val w = new PrintWriter(new File(s"graphs/$i.gv"))
        w.write(d)
        w.close()
    }
  }
}
