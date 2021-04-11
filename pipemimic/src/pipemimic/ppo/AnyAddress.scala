package pipemimic.ppo

import pipemimic.GraphTree.GraphTreeEmptyLeaf
import pipemimic._
import pipemimic.statistics.DotGraph

import scala.collection.mutable.ListBuffer

class AnyAddress(pipeline: Pipeline) extends PreservedProgramOrderVerification {
  /* generate scenarios for 4 types of program orders (with different interleaving of address) */
  private val scenariosForEachPO = programOrderToDirectionsList.view.mapValues { po =>
    val events = getEvents(po, atSameAddress = false)
    events.flatMap(AllScenariosForPO(pipeline, _))
  } .toMap

  /* verify global scenarios */
  private def verify(scenarios: List[(String, Scenario)]): (Boolean, List[DotGraph]) = {
    var allSatisfied = true /* scenarios satisfy program order */
    val graphs = ListBuffer.empty[DotGraph]

    scenarios foreach { case (title, paths) =>
      println(s"verify ppo at $title")
      val staticEdges = StaticEdges(s"PPOGlobal($title)", pipeline, paths)
      /* generate ppo global events */
      val edgesToBeVerified = { // TODO add speculative load reorder events
        require(paths.length == 2)
        (paths.headOption, paths.lastOption) match {
          case (Some(src), Some(dst)) =>
            val localCore = src.evt.iiid.proc /* core id for predecessor event */
            val allCores = src.performStages.find(_.isMainMemory) match {
              case Some(PerformStages(_, cores, _, _, _)) => cores
              case None => Nil
            }
            // FIXME when running ppo remote cores always be zero then there will be NO edges to be verified!
            val remoteCores = allCores.filterNot(_ == localCore)
            GraphTreeLeaf("PPO", PerfWRTiBeforePerfWRTj(src, dst, localCore, remoteCores))

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
