package pipemimic.ppo

import pipemimic.Stages._
import pipemimic.execution._
import pipemimic.statistics.DotGraph
import pipemimic.{CartesianProduct, Event, GlobalGraphID, GraphTree, GraphTreeLeaf, ListImprovements, Stages}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait PreservedProgramOrderVerification extends GlobalGraphID {
  /**
    * Perform with respect to other cores before with respect to localCore
    * @param po1 with respect to i in cores (other core)
    * @param po2 with respect to j (local core)
    * @param localCore index of local core
    * @param cores indices of other cores
    * @return edges in global graph
    */
  def PerfWRTiBeforePerfWRTj(po1: PathOption, po2: PathOption, localCore: Int, cores: List[Int])
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
    def PerfWRTiAtStage(l: List[PerformStages], c: Int): Option[Stages.Location] = {
      l match {
        case PerformStages(stg, cores, _, _, _) :: next =>
          if (cores.contains(c)) Some(stg) else PerfWRTiAtStage(next, c)
        case Nil => None
      }
    }

    val locationPair: List[(Option[Stages.Location], Option[Stages.Location])] = cores.map(remote =>
      /* perform with respect to remote core,      perform with respect to local core */
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

    val (title, staticEdges) = getid(p, g).flatten.head
    val (edgesLabel, ppoEdges) = getid(p, v).flatten.head

    val dotGraphs = ListBuffer.empty[DotGraph]
    var isSatisfied = true

    for (edge <- ppoEdges) {
      val (src, dst, edgeLabel) = edge
      staticEdges.existsPath(src, dst) match {
        case Unverified(_, _, _) => /* ppo edge unverified */
          isSatisfied = false
          dotGraphs += new DotGraph(s"PPO Unverified! ${p.pipeName}", staticEdges,
            ungeid(p, _), (x: Int) => GlobalEventString(p, ungeid(p, x)), List(src, dst), Nil, p.stages.length)
        case MustHappenBefore(_, l) =>
          dotGraphs += new DotGraph(s"PPO Verified: ${p.pipeName}", staticEdges, ungeid(p, _),
            (x: Int) => GlobalEventString(p, ungeid(p, x)), l, l.pairConsecutive, p.stages.length)
        case Cyclic(_, l) =>
          dotGraphs += new DotGraph(s"PPO Ruled out (cyclic): ${p.pipeName}", staticEdges,
            ungeid(p, _), (x: Int) => GlobalEventString(p, ungeid(p, x)), l, l.pairConsecutive, p.stages.length)
      }
    }

    (isSatisfied, dotGraphs.toList)
  }

  /**
    * Add title [t] to MHBResult [r]
    * @param t title
    * @param r result with current name
    * @return result with name replaced by title t
    */
  def AddTitle(t: String, r: (String, MHBResult)): (String, MHBResult) = (t, r._2)

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

  /**
    * Generate dot graph for result with name [tr]
    * @param p pipeline
    * @param tr title and result
    * @return dot graph String
    */
  def GraphOfPPOVerificationResult(p: Pipeline, tr: (String, MHBResult)): (String, String) = {
    val (t, r) = tr
    val f: Int => String = x => GlobalEventString(p, ungeid(p, x))
    (
      s"${p.pipeName}: PPO: $t",
      r match {
        case MustHappenBefore(g, l) =>
          DotGraph(s"PPO Verified: ${p.pipeName}: $t", g, ungeid(p, _), f, l,
            l.pairConsecutive, p.stages.length)
        case Unverified(g, s, d) =>
          DotGraph(s"PPO Unverified! ${p.pipeName}: $t", g, ungeid(p, _), f, List(s, d), Nil, p.stages.length)
        case Cyclic(g, l) =>
          DotGraph(s"PPO Ruled out (cyclic): ${p.pipeName}: $t", g, ungeid(p, _), f, l,
            l.pairConsecutive, p.stages.length)
      }
    )
  }
}
