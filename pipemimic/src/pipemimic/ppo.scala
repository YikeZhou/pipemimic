package pipemimic

import Stages.{GlobalEvent, GlobalEventString, PathOption, PerformStages, Pipeline, Scenario, ScenarioEdges}
import ListUtils.{LastError, NthDefault, NthError, PairConsecutive}
import MustHappenBefore.TreeMustHappenBeforeInAllGraphs
import GlobalGraphIDUtils.{getid, ungeid}
import Bell.BellNumber
import CartesianUtils.CartesianProduct
import Dot.DotGraph

import scala.annotation.tailrec

/** ppo/po-loc satisfaction tests */
object PreservedProgramOrder {

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

  /* Verification Scenarios */

  /**
    * Apply TreeMustHappenBeforeInAllGraphs to ids in graph tree g and v.
    * @param g graph produced by ScenarioEdges
    * @param v graph produced by PPOGlobalEvents
    * @param p pipeline
    * @return MHBResult
    */
  def VerifyPPOScenario(g: GraphTree[GlobalEvent], v: GraphTree[GlobalEvent], p: Pipeline)
  : List[(String, MHBResult)] = {
    TreeMustHappenBeforeInAllGraphs(getid(p, g), getid(p, v))
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
    * Given list of direction(r/w)s, return a list of event
    * @param l list of directions
    * @return list of event (labeled to processor 0)
    */
  def GenerateEventsFromDirections(l: List[Direction.Value]): List[Event] = {
    l.zipWithIndex map {
      /* Address will be adjusted by [ReplaceLocs] later, Value will be ignored */
      case (value, i) => Event(i, Iiid(0, i), Access(value, 0, 0))
    }
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
            PairConsecutive(l), p.stages.length)
        case Unverified(g, s, d) =>
          DotGraph(s"PPO Unverified! ${p.pipeName}: $t", g, ungeid(p, _), f, List(s, d), Nil, p.stages.length)
        case Cyclic(g, l) =>
          DotGraph(s"PPO Ruled out (cyclic): ${p.pipeName}: $t", g, ungeid(p, _), f, l,
            PairConsecutive(l), p.stages.length)
      }
    )
  }

  /* Interface to verify ppo/po-loc */

  /**
    * calculates the set of all possible Scenarios for a given Pipeline and given directions of Events in program
    * order
    * @param p pipeline
    * @param po given program order
    * @return all possible Scenarios
    */
  def AllScenariosForPOWithAnyAddress(p: Pipeline, po: List[Direction.Value]): List[(String, Scenario)] = {
    /**
      * Given a list of [Event]s, return a list of lists of [Event]s in which the [Location]s accessed by the [Event]s
      * are replaced by each possible assignment of overlapping vs. non-overlapping [Location]s.
      * For example, given a list of two [Event]s of directions [R] and [R], return the pair of lists (R 0, R 0) and
      * (R 0, R 1), i.e., the overlapping case and the non-overlapping case. Scenarios with more than two events will
      * have more than two possibilities.
      * @param l list of events (without location/address info)
      * @return all possible assignment of locations to events in l
      */
    def AllLocationCombos(l: List[Event]): List[List[Event]] = {
      /**
        * Replace the [Location] accessed by each [Event] in [le] with the [Location] in the corresponding position
        * in [ll].
        * @param le list of events need replacing their locations
        * @param ll list of locations
        * @return le with locations replaced by ll
        */
      def ReplaceLocs(le: List[Event], ll: List[Location]): List[Event] = {
        le zip ll map {
          case (Event(eiid, iiid, Access(d, _, v)), location) => Event(eiid, iiid, Access(d, location, v))
        }
      }
      BellNumber(l.length).map(ReplaceLocs(l, _))
    }

    val programOrder = GenerateEventsFromDirections(po) /* add direction info */
    val programOrders = AllLocationCombos(programOrder) /* add location info */
    programOrders.flatMap(AllScenariosForPO(p, _))
  }

  /**
    * Given a pipeline and type of program order (Read-after-read, etc) along with indices of events, return
    * corresponding dot graph.
    * @param p structure of pipeline
    * @param po type of program order
    * @param e1 event index in certain scenario
    * @param e2 event index in certain scenario
    * @return dot graph
    */
  def GraphsToVerifyPPOWithAnyAddresses(p: Pipeline, po: List[Direction.Value], e1: Int, e2: Int)
  : List[(String, String)] = {
    /**
      * Given a pipeline and type of program order (Read-after-read, etc) along with indices of events, return
      * corresponding MHBResult (along with its title).
      * @param p structure of pipeline
      * @param po type of program order
      * @param e1 event index in certain scenario
      * @param e2 event index in certain scenario
      * @return Must-Happen-Before Result
      */
    def VerifyPPOWithAnyAddresses(p: Pipeline, po: List[Direction.Value], e1: Int, e2: Int)
    : List[(String, MHBResult)] = {

      /* generate all possible scenarios */
      val scenarios = AllScenariosForPOWithAnyAddress(p, po)

      /**
        * Verify a list of scenario
        * @param l list of scenarios with their titles
        * @param p pipeline
        * @param e1 event 1
        * @param e2 event 2
        * @return MHBResult with scenario titles
        */
      def VerifyPPOGlobalScenarios(l: List[(String, Scenario)], p: Pipeline, e1: Int, e2: Int)
      : List[(String, MHBResult)] = {
        /**
          * Verify a single case of scenario (partly). [[VerifyPPOScenario]] does the second part.
          * @param p pipeline
          * @param s scenario
          * @param e1 event 1
          * @param e2 event 2
          * @return pair of graph TODO
          */
        def GraphToVerifyPPOGlobalScenario(p: Pipeline, s: Scenario, e1: Int, e2: Int)
        : (GraphTree[GlobalEvent], GraphTree[GlobalEvent]) = {

          def PPOGlobalEvents(s: Scenario, e1: Int, e2: Int): GraphTree[GlobalEvent] = {
            def PPOMustHappenBeforeGlobalEvents(s: Scenario, e1: Int, e2: Int): GraphTree[GlobalEvent] = {
              (NthError(s, e1), NthError(s, e2)) match {
                case (Some(po1), Some(po2)) =>
                  val localCore = po1.evt.iiid.proc
                  val ps1 = po1.performStages
                  val allCores = LastError(ps1) match {
                    case Some(PerformStages(_, cores, _, _, _)) => cores
                    case _ => Nil
                  }
                  val remoteCores = allCores.filterNot(_ == localCore) /* no remote cores here? */
                  GraphTreeLeaf("PPO", PerfWRTiBeforePerfWRTj(po1, po2, localCore, remoteCores))
                case _ => GraphTree.GraphTreeEmptyLeaf[GlobalEvent]
              }
            }
            val dirs = s.map(_.evt.dirn)
            PPOMustHappenBeforeGlobalEvents(s, e1, e2)
//            (NthDefault(e1, dirs, Direction.W), NthDefault(e2, dirs, Direction.W)) match {
//              case (Direction.R, Direction.R) => GraphTreeOr(
//                // FIXME: Missing PPOSpeculativeLoadReorderEvents
//                List(PPOMustHappenBeforeGlobalEvents(s, e1, e2))
//              )
//              case _ => PPOMustHappenBeforeGlobalEvents(s, e1, e2)
//            }
          }

          // FIXME: is this static edges? type: GlobalEvent
          val g = ScenarioEdges("PPO", p, s) /* defined in Stages, return all global edges in scenario s */
          val v = PPOGlobalEvents(s, e1, e2) /* defined above, assume to be edges need checking? */
          (g, v)
        }

        /* apply a two phase verification on scenarios in list l */
        l match {
          case (title, h) :: t =>
            val (g, v) = GraphToVerifyPPOGlobalScenario(p, h, e1, e2)
            VerifyPPOScenario(g, v, p).map(AddTitle(title, _)) ::: VerifyPPOGlobalScenarios(t, p, e1, e2)
          case Nil => Nil
        }
      }
      /* then verify each scenario */
      VerifyPPOGlobalScenarios(scenarios, p, e1, e2)
    }
    /* convert MHBResult to dot graphs */
    val lv = VerifyPPOWithAnyAddresses(p, po, e1, e2)
    lv.map(GraphOfPPOVerificationResult(p, _))
  }

  def GraphsToVerifyPPOWithSameAddress(p: Pipeline, po: List[Direction.Value], e1: Int, e2: Int): List[(String, String)] = {
    def VerifyPPOWithSameAddress(p: Pipeline, po: List[Direction.Value], e1: Int, e2: Int): List[(String, MHBResult)] = {
      def AllScenariosForPOWithSameAddress(p: Pipeline, po: List[Direction.Value]): List[(String, Scenario)] = {
        val programOrder = GenerateEventsFromDirections(po) /* all events have same location 0 */
        AllScenariosForPO(p, programOrder)
      }

      val scenarios = AllScenariosForPOWithSameAddress(p, po)

      def VerifyPPOLocalScenarios(l: List[(String, Scenario)], p: Pipeline, e1: Int, e2: Int): List[(String, MHBResult)] = {
        def GraphToVerifyPPOLocalScenario(p: Pipeline, s: Scenario, e1: Int, e2: Int): (GraphTree[GlobalEvent], GraphTree[GlobalEvent]) = {

          def PPOLocalEvents(s: Scenario, e1: Int, e2: Int): GraphTree[GlobalEvent] = {
            def PPOMustHappenBeforeLocalEvents(s: Scenario, e1: Int, e2: Int): GraphTree[GlobalEvent] = {
              (NthError(s, e1), NthError(s, e2)) match {
                case (Some(po1), Some(po2)) =>
                  val localCore = po1.evt.iiid.proc
                  GraphTreeLeaf("PPO", PerfWRTiBeforePerfWRTj(po1, po2, localCore, List(localCore)))
                case _ => GraphTree.GraphTreeEmptyLeaf[GlobalEvent]
              }
            }
            PPOMustHappenBeforeLocalEvents(s, e1, e2)
//            val dirs = s.map(_.evt.dirn)
//            (NthDefault(e1, dirs, Direction.W), NthDefault(e2, dirs, Direction.W)) match {
//              case (Direction.R, Direction.R) => GraphTreeOr(List(PPOMustHappenBeforeLocalEvents(s, e1, e2)))
//              case _ => PPOMustHappenBeforeLocalEvents(s, e1, e2)
//            }
          }

          val g = ScenarioEdges("PPOLocal", p, s)
          val v = PPOLocalEvents(s, e1, e2)
          (g, v)
        }

        val raw = l map {
          case (str, scenario) =>
            val (g, v) = GraphToVerifyPPOLocalScenario(p, scenario, e1, e2)
            VerifyPPOScenario(g, v, p).map(AddTitle(str, _))
        }
        raw.flatten
      }
      VerifyPPOLocalScenarios(scenarios, p, e1, e2)
    }
    val lv = VerifyPPOWithSameAddress(p, po, e1, e2)
    lv.map(GraphOfPPOVerificationResult(p, _))
  }

  /* TSO PPO */

  def GraphsToVerifyTSOPPO(p: Pipeline): List[(String, String)] = {
    val RR = List(Direction.R, Direction.R)
    val RW = List(Direction.R, Direction.W)
    val WR = List(Direction.W, Direction.R)
    val WW = List(Direction.W, Direction.W)

    GraphsToVerifyPPOWithAnyAddresses(p, RR, 0, 1) :::
    GraphsToVerifyPPOWithAnyAddresses(p, RW, 0, 1) :::
    GraphsToVerifyPPOWithAnyAddresses(p, WW, 0, 1) :::
    GraphsToVerifyPPOWithSameAddress(p, RR, 0, 1) :::
    GraphsToVerifyPPOWithSameAddress(p, RW, 0, 1) :::
    GraphsToVerifyPPOWithSameAddress(p, WR, 0, 1) :::
    GraphsToVerifyPPOWithSameAddress(p, WW, 0, 1) :::
    Nil
  }
}