package pipemimic.ppo

import pipemimic.GraphTree.GraphTreeEmptyLeaf
import pipemimic.statistics.DotGraph
import pipemimic._

import scala.collection.mutable.ListBuffer

class SameAddress(pipeline: Pipeline) extends PreservedProgramOrderVerification {
  /* generate scenarios for 4 types of program orders */
  private val scenariosForEachPO = programOrderToDirectionsList.view.mapValues { po =>
    val events = getEvents(po, atSameAddress = true)
    events.flatMap(AllScenariosForPO(pipeline, _))
  }.toMap

  /* verify local scenarios */
  private def verify(scenarios: List[(String, Scenario)]): (Boolean, List[DotGraph]) = {
    /* scenarios should be in scenariosForEachPO's value set */

    var allSatisfied = true /* scenarios satisfy program order */
    val graphs = ListBuffer.empty[DotGraph]

    scenarios foreach { case (title, paths) =>
//      println(s"verify ppo at $title")
      val staticEdges = StaticEdges(s"PPOLocal($title)", pipeline, paths)
      val edgesToBeVerified = {
        require(paths.length == 2)
        (paths.headOption, paths.lastOption) match {
          case (Some(src), Some(dst)) =>
            val localCore = src.evt.iiid.proc /* actually there is only one core in this situation */

            val happensInMainMemory = HappensInMainMemory(src, dst)
            GraphTreeLeaf("PPO", PerfWRTiBeforePerfWRTj(src, dst, localCore, List(localCore), happensInMainMemory))
          case _ => GraphTreeEmptyLeaf[GlobalEvent]
        }
      }
      val (satisfied, dotGraphs) = verifyScenario(staticEdges, edgesToBeVerified, pipeline)
      if (!satisfied) allSatisfied = false
      graphs.addAll(dotGraphs)
    }

    (allSatisfied, graphs.toList)
  }

  private val resultsForEachPO = scenariosForEachPO.view.mapValues(verify)

  def isSatisfied(po: ProgramOrder.Value): Boolean = resultsForEachPO(po)._1

  def getGraphs(po: ProgramOrder.Value): List[DotGraph] = resultsForEachPO(po)._2
}
