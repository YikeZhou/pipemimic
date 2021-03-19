package pipemimic

import Stages.{PerformStages, PathOption, GlobalEvent, Scenario, Pipeline, ScenarioEdges, GlobalEventString}
import ListUtils.{NthDefault, NthError, LastError, PairConsecutive}
import MustHappenBefore.TreeMustHappenBeforeInAllGraphs
import GlobalGraphIDUtils.{getid, ungeid}
import Bell.BellNumber
import CartesianUtils.CartesianProduct
import Dot.DotGraph

object PreservedProgramOrder {

  def PerfWRTiAtStage(l: List[PerformStages], c: Int): Option[Stages.Location] = {
    l match {
      case PerformStages(stg, cores, _, _, _) :: next => if (cores.contains(c)) Some(stg) else PerfWRTiAtStage(next, c)
      case Nil => None
    }
  }

  def PerfWRTiBeforePerfWRTj(po1: PathOption, po2: PathOption, localCore: Int, cores: List[Int]): List[(GlobalEvent, GlobalEvent, String)] = {
    val ps1 = po1.performStages
    val ps2 = po2.performStages
    cores match {
      case head :: next => (PerfWRTiAtStage(ps1, head), PerfWRTiAtStage(ps2, localCore)) match {
        case (Some(l1), Some(l2)) => ((l1, po1.evt.eiid), (l2, po2.evt.eiid), "PPO") :: PerfWRTiBeforePerfWRTj(po1, po2, localCore, next)
        case _ => PerfWRTiBeforePerfWRTj(po1, po2, localCore, next)
      }
      case Nil => Nil
    }
  }

  def PPOMustHappenBeforeGlobalEvents(s: Scenario, e1: Int, e2: Int): GraphTree[GlobalEvent] = {
    (NthError(s, e1), NthError(s, e2)) match {
      case (Some(po1), Some(po2)) => {
        val localCore = po1.evt.iiid.proc
        val ps1 = po1.performStages
        val allCores = LastError(ps1) match {
          case Some(PerformStages(_, cores, _, _, _)) => cores
          case _ => Nil
        }
        val remoteCores = allCores.filterNot(_ == localCore)
        GraphTreeLeaf("PPO", PerfWRTiBeforePerfWRTj(po1, po2, localCore, remoteCores))
      }
      case _ => GraphTree.GraphTreeEmptyLeaf[GlobalEvent]
    }
  }

  def PPOMustHappenBeforeLocalEvents(s: Scenario, e1: Int, e2: Int): GraphTree[GlobalEvent] = {
    (NthError(s, e1), NthError(s, e2)) match {
      case (Some(po1), Some(po2)) => {
        val localCore = po1.evt.iiid.proc
        val ps1 = po1.performStages
        GraphTreeLeaf("PPO", PerfWRTiBeforePerfWRTj(po1, po2, localCore, List(localCore)))
      }
      case _ => GraphTree.GraphTreeEmptyLeaf[GlobalEvent]
    }
  }

  def PPOGlobalEvents(s: Scenario, p: Pipeline, e1: Int, e2: Int): GraphTree[GlobalEvent] = {
    val dirs = s.map(_.evt.dirn)
    (NthDefault(e1, dirs, Direction.W), NthDefault(e2, dirs, Direction.W)) match {
      case (Direction.R, Direction.R) => GraphTreeOr(List(PPOMustHappenBeforeGlobalEvents(s, e1, e2)))
      case _ => PPOMustHappenBeforeGlobalEvents(s, e1, e2)
    }
  }

  def PPOLocalEvents(s: Scenario, p: Pipeline, e1: Int, e2: Int): GraphTree[GlobalEvent] = {
    val dirs = s.map(_.evt.dirn)
    (NthDefault(e1, dirs, Direction.W), NthDefault(e2, dirs, Direction.W)) match {
      case (Direction.R, Direction.R) => GraphTreeOr(List(PPOMustHappenBeforeLocalEvents(s, e1, e2)))
      case _ => PPOMustHappenBeforeLocalEvents(s, e1, e2)
    }
  }

  /* Verification Scenarios */
  
  def GraphToVerifyPPOGlobalScenario(p: Pipeline, s: Scenario, e1: Int, e2: Int): (GraphTree[GlobalEvent], GraphTree[GlobalEvent]) = {
    val g = ScenarioEdges("PPO", p, s)
    val v = PPOGlobalEvents(s, p, e1, e2)
    (g, v)
  }

  def GraphToVerifyPPOLocalScenario(p: Pipeline, s: Scenario, e1: Int, e2: Int): (GraphTree[GlobalEvent], GraphTree[GlobalEvent]) = {
    val g = ScenarioEdges("PPOLocal", p, s)
    val v = PPOLocalEvents(s, p, e1, e2)
    (g, v)
  }

  def VerifyPPOScenario(g: GraphTree[GlobalEvent], v: GraphTree[GlobalEvent], p: Pipeline): List[(String, MHBResult)] = {
    TreeMustHappenBeforeInAllGraphs(getid(p, g), getid(p, v))
  }

  def AddTitle(t: String, r: (String, MHBResult)): (String, MHBResult) = (t, r._2)

  def VerifyPPOGlobalScenarios(l: List[(String, Scenario)], p: Pipeline, e1: Int, e2: Int): List[(String, MHBResult)] = {
    l match {
      case (title, h) :: t => {
        val (g, v) = GraphToVerifyPPOGlobalScenario(p, h, e1, e2)
        // val _g = g
        VerifyPPOScenario(g, v, p).map(AddTitle(title, _)) ::: VerifyPPOGlobalScenarios(t, p, e1, e2)
      }
      case Nil => Nil
    }
  }

  def VerifyPPOLocalScenarios(l: List[(String, Scenario)], p: Pipeline, e1: Int, e2: Int): List[(String, MHBResult)] = {
    l match {
      case (title, h) :: t => {
        val (g, v) = GraphToVerifyPPOLocalScenario(p, h, e1, e2)
        // val _g = g
        VerifyPPOScenario(g, v, p).map(AddTitle(title, _)) ::: VerifyPPOLocalScenarios(t, p, e1, e2)
      }
      case Nil => Nil
    }
  }

  def GenerateEventsFromDirections(l: List[Direction.Value]): List[Event] = {
    def helper(l: List[Direction.Value], n: Int): List[Event] = {
      l match {
        case head :: next => Event(n, Iiid(0, n), Access(head, 0, 0)) :: helper(next, n + 1)
        case Nil => Nil
      }
    }

    helper(l, 0)
  }

  def ReplaceLocs(le: List[Event], ll: List[Location]): List[Event] = {
    (le, ll) match {
      case (Event(eeiid, eiiid, Access(d, _, v)) :: et, lh :: lt) => Event(eeiid, eiiid, Access(d, lh, v)) :: ReplaceLocs(et, lt)
      case _ => Nil
    }
  }

  def AllLocationCombos(l: List[Event]): List[List[Event]] = {
    BellNumber(l.length).map(ReplaceLocs(l, _))
  }

  def ScenarioTitle(l: List[PathOption]): String = {
    l match {
      case head :: next => next match {
        case Nil => head.optionName
        case _ => s"${head.optionName} -> ${ScenarioTitle(next)}"
      }
      case Nil => ""
    }
  }

  def AllScenariosForPO(p: Pipeline, events: List[Event]): List[(String, Scenario)] = {
    val allPaths = events.map(p.pathsFor(_))
    CartesianProduct(allPaths).map(s => (ScenarioTitle(s), s))
  }

  def AllScenariosForPOWithAnyAddress(p: Pipeline, po: List[Direction.Value]): List[(String, Scenario)] = {
    val programOrder = GenerateEventsFromDirections(po)
    val programOrders = AllLocationCombos(programOrder)
    programOrders.map(AllScenariosForPO(p, _)).flatten
  }

  def AllScenariosForPOWithSameAddress(p: Pipeline, po: List[Direction.Value]): List[(String, Scenario)] = {
    val programOrder = GenerateEventsFromDirections(po)
    AllScenariosForPO(p, programOrder)
  }

  def VerifyPPOWithAnyAddresses(p: Pipeline, po: List[Direction.Value], e1: Int, e2: Int): List[(String, MHBResult)] = {
    val scenarios = AllScenariosForPOWithAnyAddress(p, po)
    VerifyPPOGlobalScenarios(scenarios, p, e1, e2)
  }

  def VerifyPPOWithSameAddress(p: Pipeline, po: List[Direction.Value], e1: Int, e2: Int): List[(String, MHBResult)] = {
    val scenarios = AllScenariosForPOWithSameAddress(p, po)
    VerifyPPOLocalScenarios(scenarios, p, e1, e2)
  }

  def GraphOfPPOVerificationResult(p: Pipeline, e1: Int, e2: Int, tr: (String, MHBResult)): (String, String) = {
    val (t, r) = tr
    val f: Int => String = x => GlobalEventString(p, ungeid(p, x))
    (
      s"${p.pipeName}: PPO: $t",
      r match {
        case MustHappenBefore(g, l) => DotGraph(s"PPO Verified: ${p.pipeName}: $t", g, ungeid(p, _), f, l, PairConsecutive(l), p.stages.length)
        case Unverified(g, s, d) => DotGraph(s"PPO Unverified! ${p.pipeName}: $t", g, ungeid(p, _), f, List(s, d), Nil, p.stages.length)
        case Cyclic(g, l) => DotGraph(s"PPO Ruled out (cyclic): ${p.pipeName}: $t", g, ungeid(p, _), f, l, PairConsecutive(l), p.stages.length)
      }
    )
  }

  def GraphsToVerifyPPOWithAnyAddresses(p: Pipeline, po: List[Direction.Value], e1: Int, e2: Int): List[(String, String)] = {
    val lv = VerifyPPOWithAnyAddresses(p, po, e1, e2)
    lv.map(GraphOfPPOVerificationResult(p, e1, e2, _))
  }

  def GraphsToVerifyPPOWithSameAddress(p: Pipeline, po: List[Direction.Value], e1: Int, e2: Int): List[(String, String)] = {
    val lv = VerifyPPOWithSameAddress(p, po, e1, e2)
    lv.map(GraphOfPPOVerificationResult(p, e1, e2, _))
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