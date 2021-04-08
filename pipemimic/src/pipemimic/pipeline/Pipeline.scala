package pipemimic

import edges.StaticEdges
import results.{LitmusResult, PPOResult}

import scala.annotation.tailrec

/**
  * A [Pipeline] is defined as a set of [Stage]s, a function [pathsFor] that maps each event into a list of its
  * possible [PathOptions]
  * @param pipeName name of this pipeline
  * @param stages stages in this pipeline
  * @param pathsFor maps given event into a list of PathOptions
  */
case class Pipeline(pipeName: String, stages: List[Stage], pathsFor: Event => PathOptions) {
  // TODO add helper functions to build stages, pathsFor etc
  var cores: Int = 1 // FIXME these three values must be configurable
  var pipelineStageCnt = 5
  var memStageCnt = 2

  /* return nth bell number */
  def BellNumber(n: Int): List[List[Int]] = {
    require(n >= 0)

    /* return unique values in l then append l's length at the end */
    def UniqueValues(l: List[Int]): List[Int] = {

      /* extract unique values from l to r */
      @tailrec
      def helper(l: List[Int], r: List[Int]): List[Int] = {
        l match {
          case Nil => r
          case head :: next => helper(next, r.addUnique(head))
        }
      }

      val r = helper(l, List())
      r.appended(r.length)
    }

    def helper(l: List[Int]): List[List[Int]] = {
      UniqueValues(l).map(l.appended)
    }
    if (n == 0) List(List.empty[Int])
    else BellNumber(n - 1).map(helper).foldLeft(List.empty[List[Int]])((l, as) => l ::: as)
  }

  /**
    * Given list of direction(r/w)s, return a list of event
    * @param l list of directions
    * @return list of event (labeled to processor 0)
    */
  private def generateEventsFromDirections(l: List[Direction.Value]): List[Event] = {
    l.zipWithIndex map {
      /* Address will be adjusted by [ReplaceLocs] later, Value will be ignored */
      case (value, i) => Event(i, Iiid(0, i), Access(value, 0, 0))
    }
  }

  /**
    * calculates the set of all possible Scenarios for a given Pipeline and given directions of Events in program order
    * @param p pipeline -> use pathsFor function to determine path for given event
    * @param events list of events
    * @return all possible scenarios
    */
  def allScenariosForPO(p: Pipeline, events: List[Event]): List[(String, Scenario)] = {
    def ScenarioTitle(l: List[PathOption]): String = {
      l.map(_.optionName).mkString(" -> ")
    }
    val allPaths = events.map(p.pathsFor(_))
    CartesianProduct(allPaths).map(s => (ScenarioTitle(s), s))
  }

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

  def verifyPreservedProgramOrderWithAnyAddresses(po: List[Direction.Value]): List[PPOResult] = {
    /* number of events in uhb graph */
    val eventCnt = po.length
    assert(eventCnt == 2)


    /**
      * Given a list of [Event]s, return a list of lists of [Event]s in which the [Location]s accessed by the [Event]s
      * are replaced by each possible assignment of overlapping vs. non-overlapping [Location]s.
      * For example, given a list of two [Event]s of directions [R] and [R], return the pair of lists (R 0, R 0) and
      * (R 0, R 1), i.e., the overlapping case and the non-overlapping case. Scenarios with more than two events will
      * have more than two possibilities.
      * @param l list of events (without location/address info)
      * @return all possible assignment of locations to events in l
      */
    def allLocationCombos(l: List[Event]) = {
      BellNumber(l.length) map { combo =>
        combo zip l map {
          case (addr, event) => event match {
            case Event(eiid, iiid, Access(d, _, v)) => Event(eiid, iiid, Access(d, addr, v))
          }
        }
      }
    }


    /* generate all possible scenarios */
    val programOrderWithoutAddresses = generateEventsFromDirections(po)
    val programOrder = allLocationCombos(programOrderWithoutAddresses)
    val scenarios = programOrder.flatMap(allScenariosForPO(this, _))


    /* then verify each scenario */

    def ppoMHBGlobalEvents(s: Scenario): GraphTree[GlobalEvent] = {
      (s.headOption, s.lift(1)) match {
        case (Some(poi), Some(poj)) =>
          val localCore = poi.evt.iiid.proc
          val allCores = poi.performStages.flatMap(_.cores)
          val remoteCores = allCores.filterNot(_ == localCore)
          GraphTreeLeaf("PPO", PerfWRTiBeforePerfWRTj(poi, poj, localCore, remoteCores))
        case _ => GraphTree.GraphTreeEmptyLeaf[GlobalEvent]
      }
    }

    /* apply a two phase verification on scenarios in list l */
    scenarios map { case (title, scenario) =>
      val staticEdges = StaticEdges.scenarioStaticEdges(title, this, scenario)
      /* calculate global ppo events */
      val edgesToBeVerified = ppoMHBGlobalEvents(scenario)

      val ppoResult = new PPOResult(GlobalGraphIDUtils.getid(this, staticEdges),
        GlobalGraphIDUtils.getid(this, edgesToBeVerified))
      ppoResult
    }
  }

  def verifyPreservedProgramOrderWithSameAddress(po: List[Direction.Value]): List[PPOResult] = {
    /* all events have same address 0 */
    val programOrder = generateEventsFromDirections(po)
    val scenarios = allScenariosForPO(this, programOrder)

    def ppoLocalEvents(s: Scenario) = {
      def ppoMHBLocalEvents = {
        s match {
          case List(po1, po2) =>
            val localCore = po1.evt.iiid.proc
            GraphTreeLeaf("PPO", PerfWRTiBeforePerfWRTj(po1, po2, localCore, List(localCore)))
          case _ => GraphTree.GraphTreeEmptyLeaf[GlobalEvent]
        }
      }
      ppoMHBLocalEvents
    }

    /* verify ppo local scenarios */
    scenarios map { case (title, scenario) =>
      val staticEdges = StaticEdges.scenarioStaticEdges("PPOLocal", this, scenario)
      val edgesToBeVerified = ppoLocalEvents(scenario)

      new PPOResult(GlobalGraphIDUtils.getid(this, staticEdges),
        GlobalGraphIDUtils.getid(this, edgesToBeVerified))
    }
  }

  def executeLitmusTest(test: List[Event], rf: List[(Eiid, Eiid)]): LitmusResult = ???
}

object Pipeline {
  /* Local Reordering */

  /**
    * any ordering guaranteed at the input is also guaranteed at the output. This is the common case.
    */
  val FIFO: LocalReordering = _ => ordering => ordering

  /**
    * Operations can leave the stage in any order; nothing is guaranteed.
    */
  val NoOrderGuarantees: LocalReordering = _ => _ => Nil

  /**
    * The output order is guaranteed to match some previous ordering
    */
  def Restore(n: Int): LocalReordering = e => _ => e.nthDefault(n, Nil)

  /* Special Edge Maps */

  /**
    * In most cases, we don't need to add any special edges
    */
  val NoSpecialEdges: SpecialEdgeMap = _ => _ => _ => Nil

  /* TODO add parser and factory method here */

}