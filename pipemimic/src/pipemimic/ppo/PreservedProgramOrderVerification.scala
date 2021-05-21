package pipemimic.ppo

import pipemimic.execution._
import pipemimic.statistics.DotGraph
import pipemimic._
import pipemimic.topology.VerifyMustHappenBeforeInGraph

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait PreservedProgramOrderVerification extends GlobalGraphID {
  /**
    * Perform with respect to other cores before with respect to localCore
    * @param po1 with respect to i in cores (other core)
    * @param po2 with respect to j (local core)
    * @param localCore index of local core
    * @param cores indices of other cores
    * @param isMainMemory restrict perform stage we chosen
    * @return edges in global graph
    */
  def PerfWRTiBeforePerfWRTj(po1: PathOption, po2: PathOption, localCore: Int, cores: List[Int], isMainMemory: Option[Boolean])
  : List[(GlobalEvent, GlobalEvent, String)] = {
    /**
      * perform stage with respect to i: Given a list of perform stages [l](containing performing locations with respect
      * to different cores) and a core index [c], find the perform stage where its list of cores contains [c], then
      * return the location [stage].
      * @param l list of perform stages
      * @param c core index
      * @return performing location with respect to core c
      */
    @tailrec
    def PerfWRTiAtStage(l: List[PerformStages], c: Int): Option[Location] = {
      l match {
        case PerformStages(stg, cores, _, _, im) :: next =>
          if (cores.contains(c) && ((isMainMemory.isDefined && isMainMemory.contains(im)) || isMainMemory.isEmpty))
            Some(stg)
          else
            PerfWRTiAtStage(next, c)
        case Nil => None
      }
    }

    val locationPair: List[(Option[Location], Option[Location])] = cores.map(remote =>
      /* local event perform with respect to remote core, remote event perform with respect to local core */
      (PerfWRTiAtStage(po1.performStages, remote), PerfWRTiAtStage(po2.performStages, localCore))
    ).filter(t => t._1.isDefined && t._2.isDefined)
    /* local core happens before [c] in cores */
    locationPair.map(x => ((x._1.get, po1.evt.eiid), (x._2.get, po2.evt.eiid), "PPO"))
  }

  /**
    * check if all edges in v are satisfied in g
    * @param g graph produced by ScenarioEdges
    * @param v graph produced by PPOGlobalEvents(ppo edges)
    * @param p pipeline
    * @return graph title + MHBResult
    */
  def verifyScenario(g: GraphTree[GlobalEvent], v: GraphTree[GlobalEvent], p: Pipeline): (Boolean, List[DotGraph]) = {
    require(g.isInstanceOf[GraphTreeLeaf[GlobalEvent]] && v.isInstanceOf[GraphTreeLeaf[GlobalEvent]])

    val (staticEdgesTitles, staticEdgesList) = globalGraphID(p, g).flatten.unzip
    val (_, ppoEdgesList) = globalGraphID(p, v).flatten.unzip

    val dotGraphs = ListBuffer.empty[DotGraph]
    var isSatisfied = true

    val eventPairName = staticEdgesTitles.head

    staticEdgesList zip ppoEdgesList foreach { case (staticEdges, ppoEdges) =>
      for (edge <- ppoEdges) {
        val (src, dst, edgeLabel) = edge
        staticEdges.existsPath(src, dst) match {
          case Unverified(_, _, _) => /* ppo edge unverified */
            isSatisfied = false
            dotGraphs += new DotGraph(s"PPO Unverified! ${p.pipeName} $eventPairName", staticEdges,
              ungeid(p, _), (x: Int) => GlobalEventString(p, ungeid(p, x)), List(src, dst), Nil, p.stages.length)
          case MustHappenBefore(_, l) =>
            dotGraphs += new DotGraph(s"PPO Verified: ${p.pipeName} $eventPairName", staticEdges, ungeid(p, _),
              (x: Int) => GlobalEventString(p, ungeid(p, x)), l, l.pairConsecutive, p.stages.length)
          case Cyclic(_, l) =>
            dotGraphs += new DotGraph(s"PPO Ruled out (cyclic): ${p.pipeName} $eventPairName", staticEdges,
              ungeid(p, _), (x: Int) => GlobalEventString(p, ungeid(p, x)), l, l.pairConsecutive, p.stages.length)
        }
      }
    }

    (isSatisfied, dotGraphs.toList)
  }

  /**
    * calculates the set of all possible Scenarios for a given Pipeline and given directions of Events in program order
    * @param p pipeline -> use pathsFor function to determine path for given event
    * @param events list of events
    * @return all possible scenarios
    */
  def AllScenariosForPO(p: Pipeline, events: List[Event]): List[(String, Scenario)] = {
    def ScenarioTitle(l: List[PathOption]): String = {
      l.map(_.optionName).mkString(" -> ")
    }
    val allPaths = events.map(p.pathsFor(_))
    CartesianProduct(allPaths).map(s => (ScenarioTitle(s), s))
  }

  /* judge if event happens in main memory
   * there must be at least 1 entry in performStages of [write] that happens in main memory
   * and can be taken as sign of a completed store
   * However, read has two types: normal and store buffer forwarding
   * and normal's isMainMemory is always true while stbForward's always false */
  def HappensInMainMemory(src: PathOption, dst: PathOption): Option[Boolean] = {
    if (src.evt.isRead && dst.evt.isRead) {
      /* don't care */
      None
    } else if (src.evt.isRead) {
      Some(src.performStages.head.isMainMemory)
    } else if (dst.evt.isRead) {
      Some(dst.performStages.head.isMainMemory)
    } else { /* both write events */
      Some(true)
    }
  }
}
