package pipemimic

import pipemimic.Adjacency.Dijkstra
import pipemimic.CartesianUtils._
import pipemimic.Dot.DotGraph
import pipemimic.GlobalGraphIDUtils._
import pipemimic.GraphTree.{DNFOfTree, DNFStringOfTree, TreeOfDNF}
import pipemimic.Interleavings.Interleave
import pipemimic.MustHappenBefore._
import pipemimic.Stages._

import scala.annotation.tailrec

object Execution {

  /* Calculation of uhb graphs */

  /**
    * Return the PathOption for given Event in Scenario s
    * @param s scenario
    * @param e event
    * @return PathOption for e
    */
  @tailrec
  def pathOfEvent(s: Scenario, e: Eiid): Option[PathOption] = {
    s match {
      case head :: next => if (head.evt.eiid == e) Some(head) else pathOfEvent(next, e)
      case Nil => None
    }
  }

  /**
    * Given the list of [PerformStages] for an event, and a core [c], calculate the set of stages at the event has
    * performed with respect to core [c]
    * @param c core index
    * @param po path option (including different perform stages for corresponding cores)
    * @return perform stage (location index) FIXME: can it return multiple stages?
    */
  def PerformStagesWRTCore(c: Int, po: PathOption): List[Int] = {
    /**
      * Given the list of [PerformStages] for an event, and a core [c], calculate the set of stages at the event has
      * performed with respect to core [c]
      * @param c core index
      * @param l list of perform stage
      * @return list of perform stages index
      */
    def helper(c: Int, l: List[PerformStages]): List[Int] = {
      l match {
        case PerformStages(stage, cores, _, _, _) :: next =>
          if (cores.contains(c)) stage :: helper(c, next) else helper(c, next)
        case Nil => Nil
      }
    }

    helper(c, po.performStages)
  }

  /**
    * Like PerformStagesWRTCore, but handles cacheLineInvLoc
    * @param c core index
    * @param po path option (provides perform stages)
    * @return list of core indices
    */
  def PerformOrInvStagesWRTCore(c: Int, po: PathOption): List[Int] = {
    /**
      *
      * @param c core index
      * @param l list of perform stages
      * @return list of cores
      */
    def helper(c: Int, l: List[PerformStages]): List[Int] = {
      l match {
        case PerformStages(stage, cores, _, None, _) :: next => /* without cache line invalid location, attach stage */
          if (cores.contains(c)) stage :: helper(c, next) else helper(c, next)
        case PerformStages(_, cores, _, Some(i), _) :: next => /* with a invalid location, attach this instead of stg */
          if (cores.contains(c)) i :: helper(c, next) else helper(c, next)
        case Nil => Nil
      }
    }

    helper(c, po.performStages)
  }

  /**
    * Given the list of [PerformStages] for an event, and a core [c], calculate the set of stages at which core [c]
    * can observe that the event has performed.
    * @param c core index
    * @param po path
    * @return
    */
  def VisibleStagesWRTCore(c: Int, po: PathOption): List[Int] = {
    def helper(c: Int, l: List[PerformStages]): List[Int] = {
      l match {
        case PerformStages(stage, _, cores, _, _) :: next =>
          if (cores.contains(c)) stage :: helper(c, next) else helper(c, next)
        case Nil => Nil
      }
    }

    helper(c, po.performStages)
  }

  def RFPerformPairs(src: PathOption, dst: PathOption): List[(Stages.Location, Stages.Location)] = {
    val srcCore = src.evt.iiid.proc
    val dstCore = dst.evt.iiid.proc
    val srcPerfStages = PerformStagesWRTCore(dstCore, src)
    val dstPerfStages = VisibleStagesWRTCore(srcCore, dst)
    CartesianProductPairs(srcPerfStages, dstPerfStages)
  }

  def FRInitialPerformPairs(src: PathOption, dst: PathOption): List[(Stages.Location, Stages.Location)] = {
    val srcCore = src.evt.iiid.proc
    val dstCore = dst.evt.iiid.proc
    val srcPerfStages = PerformOrInvStagesWRTCore(dstCore, src)
    val dstPerfStages = PerformStagesWRTCore(srcCore, dst)
    CartesianProductPairs(srcPerfStages, dstPerfStages)
  }

  def FRfwPerformPairs(src: PathOption, dst: PathOption): List[(Stages.Location, Stages.Location)] = {
    val srcCore = src.evt.iiid.proc
    val dstCore = dst.evt.iiid.proc
    val srcPerfStages = PerformOrInvStagesWRTCore(dstCore, src)
    val dstPerfStages = PerformStagesWRTCore(srcCore, dst)
    CartesianProductPairs(srcPerfStages, dstPerfStages)
  }

  def ReachableVertices(p: Pipeline, src: GlobalEvent, g: List[(GlobalEvent, GlobalEvent, String)])
  : List[GlobalEvent] = {
    val g2 = g.map(gepid(p, _))
    val g3 = g2.map(x => (x._1, x._2))
    val (reachable, _) = Dijkstra(g3, geid(p, src))
    reachable.map(ungeid(p, _))
  }

  def nthEventInScenarioIsWrite(s: Scenario, ge: (Int, Eiid)): Boolean = {
    s.lift(ge._2) match {
      case Some(po) => po.evt.isWrite
      case None => false
    }
  }

  def VertexHasSameAddress(s: Scenario, l: Location, e: GlobalEvent): Boolean = {
    s.lift(e._2) match {
      case Some(po) => po.evt.loc == l
      case None => false
    }
  }

  def ReachableVerticesAtSameLocation(p: Pipeline, s: Scenario, src: GlobalEvent,
                                      g: List[(GlobalEvent, GlobalEvent, String)]): List[Eiid] = {
    s.lift(src._2) match {
      case Some(po) => ReachableVertices(p, src, g)
                        .filter(VertexHasSameAddress(s, po.evt.loc, _))
                        .filter(nthEventInScenarioIsWrite(s, _))
                        .map(_._2)
                        .toSet
                        .filterNot(_ == src._2)
                        .toList
      case None => Nil
    }
  }

  def WritesToSameLocation(l: Location, s: Scenario): List[PathOption] = {
    s.filter(x => if (x.evt.dirn == Direction.R) false else l == x.evt.loc)
  }

  def ExecutionEdgeLabel(n: String, l: List[(GlobalEvent, GlobalEvent, String)]): String = {
    l match {
      case head :: next =>
        val e1 = head._1._2
        val e2 = head._2._2
        s"($e1-$n->$e2) ${ExecutionEdgeLabel(n, next)}"
      case Nil => ""
    }
  }

  def ExecutionOrderEdges_FR_initial(p: Pipeline, src: PathOption, gUhb: GraphTree[GlobalEvent], dst: PathOption)
  : GraphTree[GlobalEvent] = {
    def helper(p: Pipeline, es: Eiid, ed: Eiid, ll: (Stages.Location, Stages.Location)): GraphTree[GlobalEvent] = {
      val (ls, ld) = ll
      val s = (ls, es)
      val d = (ld, ed)
      val l = List((s, d, "FRi"))
      def PrintPossibility: GraphTree[GlobalEvent] => String = t => DNFStringOfTree(GlobalEventString(p, _), t) + '\n'
      GraphTreeLeaf(ExecutionEdgeLabel("fr", l), l)
    }

    GraphTreeAnd(gUhb :: FRInitialPerformPairs(src, dst).map(helper(p, src.evt.eiid, dst.evt.eiid, _)))
  }

  def ScenarioExecutionEdges_FR_initial(p: Pipeline, s: Scenario, gUhb: GraphTree[GlobalEvent], e: Eiid)
  : GraphTree[GlobalEvent] = {
    pathOfEvent(s, e) match {
      case Some(pr) =>
        /* Found PathOptions for r */ WritesToSameLocation(pr.evt.loc, s).foldLeft(gUhb)((b, po) =>
        ExecutionOrderEdges_FR_initial(p, pr, b, po))
      case None => sys.error("ScenarioExecutionEdges_FR_initial: event is not actually in scenario")
    }
  }

  /**
    * Given a uhb RF edge (w, r), for all vertices w' in [g_uhb] such that (w, w') is an edge in [g_uhb] between events
    * at the same location, add the FR edge (r, w').
    * @param p pipeline
    * @param s scenario
    * @param w write event
    * @param r read event
    * @param gUhb uhb graph
    * @return graph added FR edge
    */
  def ExecutionOrderEdges_FR_fromwrite(p: Pipeline, s: Scenario, w: GlobalEvent, r: GlobalEvent,
                                       gUhb: GraphTree[GlobalEvent]): GraphTree[GlobalEvent] = {
    def ___helper(s: Scenario, l: List[Eiid]): List[PathOption] = {
      l match {
        case head :: next => pathOfEvent(s, head) match {
          case Some(p) => p :: ___helper(s, next)
          case None =>         ___helper(s, next)
        }
        case Nil => Nil
      }
    }

    def __helper(r: PathOption, w: PathOption): List[(GlobalEvent, GlobalEvent, String)] = {
      FRfwPerformPairs(r, w).map(p => ((p._1, r.evt.eiid), (p._2, w.evt.eiid), "FRfw"))
    }

    def _helper(p: Pipeline, s: Scenario, w: GlobalEvent, r: GlobalEvent,
                nl: (String, List[(GlobalEvent, GlobalEvent, String)]))
    : (String, List[(GlobalEvent, GlobalEvent, String)]) = {
      val (n, l) = nl
      val wReachable = ReachableVerticesAtSameLocation(p, s, w, l)
      val _wReachable = ___helper(s, wReachable)
      pathOfEvent(s, r._2) match {
        case Some(p) =>
          val _l = _wReachable.flatMap(__helper(p, _))
          (n, l ::: _l)
        case None => nl
      }
    }

    TreeOfDNF(DNFOfTree(gUhb).map(_helper(p, s, w, r, _)))
  }

  /**
    * Given a source and destination event for an architectural RF edge, create the corresponding uhb edge(s)
    * @param p pipeline
    * @param s scenario
    * @param gUhb uhb graph
    * @param src source path
    * @param dst destination path
    * @return graph
    */
  def ExecutionOrderEdges_RFandFR_fromwrite(p: Pipeline, s: Scenario, gUhb: GraphTree[GlobalEvent],
                                            src: PathOption, dst: PathOption): GraphTree[GlobalEvent] = {
    /**
      * Given a source and destination event for a particular uhb interpretation of an RF edge, create the corresponding
      * uhb edge(s)
      * @param p pipeline
      * @param s scenario
      * @param gUhb uhb graph
      * @param es event source
      * @param ed event destination
      * @param ll location tuple
      * @return graph
      */
    def helper(p: Pipeline, s: Scenario, gUhb: GraphTree[GlobalEvent], es: Eiid, ed: Eiid,
               ll: (Stages.Location, Stages.Location)): GraphTree[GlobalEvent] = {
      val (ls, ld) = ll
      val src = (ls, es)
      val dst = (ld, ed)
      val l = List((src, dst, "RF"))
      def PrintPossibility: GraphTree[GlobalEvent] => String = t => DNFStringOfTree(GlobalEventString(p, _), t) + '\n'
      println(PrintPossibility(GraphTreeLeaf("rf_uhb", l)))
      GraphTreeAnd(List(
        ExecutionOrderEdges_FR_fromwrite(p, s, src, dst, gUhb),
        GraphTreeLeaf(ExecutionEdgeLabel("rf", l), l)
      ))
    }

    val rfPossibilities = RFPerformPairs(src, dst).map(helper(p, s, gUhb, src.evt.eiid, dst.evt.eiid, _))
    println(s"Source path ${src.optionName}, Dest path ${dst.optionName}\n")
    println(s"Architectural RF edge: ${rfPossibilities.length} uhb candidates\n")
    GraphTreeOr(rfPossibilities)
  }

  def ScenarioExecutionEdges_RF_fromwrite(p: Pipeline, s: Scenario, gUhb: GraphTree[GlobalEvent],
                                          rf: (Eiid, Eiid)): GraphTree[GlobalEvent] = {
    val (w, r) = rf
    (pathOfEvent(s, w), pathOfEvent(s, r)) match {
      case (Some(pw), Some(pr)) => ExecutionOrderEdges_RFandFR_fromwrite(p, s, gUhb, pw, pr)
      case _ => sys.error("ScenarioExecutionEdges_RF_fromwrite: event not in scenario")
    }
  }

  /* ReadsFromInitial */

  /**
    * Given: events: the list of all events in the scenario
    * Given: rf_fromwrite: the list of all rf edges which have a non-initial write as their source
    * Return: the list of all events which are reads from the initial state (i.e., those reads which are not in rf_fromwrite)
    * @param events list of event
    * @param rfFromWrite list of edge
    * @return list of events
    */
  def ReadsFromInitial(events: List[Event], rfFromWrite: List[(Eiid, Eiid)]): List[Eiid] = {
    val edgeDests = rfFromWrite.map(_._2) /* read event */
    events match {
      case head :: next => (edgeDests.contains(head.eiid), head.dirn) match {
        case (false, Direction.R) => /* head: read event from init */ head.eiid :: ReadsFromInitial(next, rfFromWrite)
        case _ => ReadsFromInitial(next, rfFromWrite)
      }
      case Nil => Nil
    }
  }

  def ScenarioExecutionEdges_RF(p: Pipeline, gUhb: GraphTree[GlobalEvent], s: Scenario,
                                rfFromWrite: List[(Eiid, Eiid)]): GraphTree[GlobalEvent] = {
    val rfInitialReads = ReadsFromInitial(s.map(_.evt), rfFromWrite)
    val gAfterRf = rfFromWrite.foldLeft(gUhb)((b, ee) => ScenarioExecutionEdges_RF_fromwrite(p, s, b, ee))
    rfInitialReads.foldLeft(gAfterRf)((g, e) => ScenarioExecutionEdges_FR_initial(p, s, g, e))
  }

  /* ScenarioExecutionEdges_WS */

  /**
    * To calculate all of the WS edges, we do the following:
    *
    * 1) Sort the events in the scenario by location
    * 2) Sort each per-location list of events by issuing core, so that we have a list
    * of lists of events in per-location, per-core program order
    * 3) Calculate the set of all interleavings of stores to each location among the
    * different cores
    * 4) Generate the list of WS edges for each interleaving
    * 5) Combine the results for each individual location into a GraphTree
    *
    * @param s scenario
    * @return list of lists of global events
    */
  def ScenarioExecutionEdges_WS_SortByCore(s: Scenario): List[List[GlobalEvent]] = {
    s match {
      case head :: next => head.performStages.lastOption match {
        case Some(PerformStages(l, _, _, _, _)) => ScenarioExecutionEdges_WS_SortByCore(next).appendToNth(
          head.evt.iiid.proc, (l, head.evt.eiid))
        case None => ScenarioExecutionEdges_WS_SortByCore(next)
      }
      case Nil => Nil
    }
  }

  def ScenarioExecutionEdges_WS_SortByLoc(s: Scenario): List[List[PathOption]] = {
    s match {
      case head :: next => (head.evt.dirn, head.evt.loc) match {
        case (Direction.W, l) => ScenarioExecutionEdges_WS_SortByLoc(next).appendToNth(l, head)
        case _ => ScenarioExecutionEdges_WS_SortByLoc(next)
      }
      case Nil => Nil
    }
  }

  def ScenarioExecutionEdges_WS_SortByLocThenCore(s: Scenario): List[List[List[GlobalEvent]]] = {
    ScenarioExecutionEdges_WS_SortByLoc(s).map(ScenarioExecutionEdges_WS_SortByCore)
  }

  def ScenarioExecutionEdges_WS_Interleavings(s: Scenario): List[List[List[GlobalEvent]]] = {
    ScenarioExecutionEdges_WS_SortByLocThenCore(s).map(Interleave)
  }

  def ScenarioExecutionEdges_WS_EdgesPerInterleaving(s: Scenario)
  : List[List[List[(GlobalEvent, GlobalEvent, String)]]] = {
    val i = ScenarioExecutionEdges_WS_Interleavings(s)
    i.map(_.map(_.pairConsecutive("WS")))
  }

  def ScenarioExecutionEdges_WS_EdgesPerLocation(l: List[List[(GlobalEvent, GlobalEvent, String)]])
  : GraphTree[GlobalEvent] = {
    val MakeLeaf: List[(GlobalEvent, GlobalEvent, String)] =>
      GraphTreeLeaf[GlobalEvent] = l =>
      GraphTreeLeaf(ExecutionEdgeLabel("ws", l), l)
    val _l = l.map(MakeLeaf)
    println(s"WS @ location: ${_l.length} candidates\n")
    GraphTreeOr(_l)
  }

  def ScenarioExecutionEdges_WS(s: Scenario): GraphTree[GlobalEvent] = {
    val l = ScenarioExecutionEdges_WS_EdgesPerInterleaving(s)
    GraphTreeAnd(l.map(ScenarioExecutionEdges_WS_EdgesPerLocation))
  }

  def ScenarioExecutionEdges(p: Pipeline, s: Scenario, rf: List[(Eiid, Eiid)], gUhb: GraphTree[GlobalEvent])
  : GraphTree[GlobalEvent] = {
    val ms = new TinyTimer("ScenarioExecutionEdges")
    ms.reset()

    val _g = GraphTreeAnd(List(gUhb, ScenarioExecutionEdges_WS(s)))
    val result = ScenarioExecutionEdges_RF(p, _g, s, rf)
    println(ms)
    result
  }

  /* Verification */

  def GraphForScenarioAcyclic(t: String, s: Scenario, p: Pipeline, rf: List[(Eiid, Eiid)]): GraphTree[GlobalEvent] = {
    val e = ScenarioEdges(t, p, s)
    ScenarioExecutionEdges(p, s, rf, e)
  }

  def GraphCheckIfAcyclic(p: Pipeline, g: GraphTree[GlobalEvent]): (List[(String, MHBResult)], Boolean, Int) = {
    TreeAcyclicInSomeGraph(getid(p, g))
  }

  def VerifyExecutionScenario(p: Pipeline, s: (String, Scenario), rf: List[(Eiid, Eiid)])
  : (List[(String, MHBResult)], Boolean, Int) = {
    def ScenarioCheckAcyclic(t: String, s: Scenario, p: Pipeline, rf: List[(Eiid, Eiid)])
    : (List[(String, MHBResult)], Boolean, Int) = {
      GraphCheckIfAcyclic(p, GraphForScenarioAcyclic(t, s, p, rf))
    }
    ScenarioCheckAcyclic(s._1, s._2, p, rf)
  }

  /**
    * Given pipeline and a list of events, calculate paths for all events and
    * @param p pipeline
    * @param events list of events
    * @return all possible scenarios
    */
  def ScenariosForEvents(p: Pipeline, events: List[Event]): List[(String, Scenario)] = {
    /**
      * Given a list of PathOption, connect these path options' names with '->'
      * @param l list of path option
      * @return formatted string of names of given path options
      */
    def ScenarioTitle(l: List[PathOption]): String = {
      l.map(_.optionName).mkString(" -> ")
    }

    val pathsForEvent: Event => PathOptions = e => p.pathsFor(e)
    val allPaths = events.map(pathsForEvent)
    CartesianProduct(allPaths).map(s => (ScenarioTitle(s), s))
  }

  /**
    * Given a pipeline, events and list of reads from edges (generated in litmus.scala), return verify result.
    * @param p pipeline
    * @param events list of events
    * @param rf list of reads from edges
    * @return MHBResult
    */
  def VerifyExecution(p: Pipeline, events: List[Event], rf: List[(Eiid, Eiid)])
  : (Boolean, Int, List[(String, MHBResult)]) = {
    /**
      * Compared to function [[VerifyExecution]], add parameters ls, lr and n.
      * @param p pipeline
      * @param events list of events
      * @param rf list of reads from edges
      * @param ls list of scenarios
      * @param lr list of results (filled in during recurrence)
      * @param n counter:
      * @return
      */
    @tailrec
    def helper(p: Pipeline, events: List[Event], rf: List[(Eiid, Eiid)],
               ls: List[(String, Scenario)], lr: List[(String, MHBResult)], n: Int)
    : (Boolean, Int, List[(String, MHBResult)]) = {
      ls match {
        case head :: next =>
          val (r, b, _n) = VerifyExecutionScenario(p, head, rf)
          if (b) (true, n + _n, r) else helper(p, events, rf, next, lr ::: r, n + _n)
        case Nil => (false, n, lr) /* TRUE cases not found */
      }
    }

    val scenarios = ScenariosForEvents(p, events) /* all combinations of events' path */
    println(s"Found ${scenarios.length} scenarios\n")
    helper(p, events, rf, scenarios, Nil, 0)
  }

  def GraphOfExecutionVerificationResult(title: String, p: Pipeline, nr: (String, MHBResult)): (String, String) = {
    val (n, r) = nr
    val f: Int => String = x => GlobalEventString(p, ungeid(p, x))
    val _n = s"${p.pipeName}: $title: $n"
    (
      _n,
      r match {
        case Unverified(g, _, _) =>
          DotGraph(s"Permitted: ${_n}", g, ungeid(p, _), f, Nil, Nil, p.stages.length)
        case MustHappenBefore(g, _) =>
          DotGraph(s"Permitted: ${_n}", g, ungeid(p, _), f, Nil, Nil, p.stages.length)
        case Cyclic(g, l) =>
          DotGraph(s"Forbidden: ${_n}", g, ungeid(p, _), f, l, l.pairConsecutive, p.stages.length)
      }
    )
  }

  def PrintRFList(l: List[(Eiid, Eiid)]): String = {
    "RF List for execution: " + l.map {
      case (e1, e2) => s"($e1-rf->$e2)"
    }.mkString(" ").appended('\n')
  }

  /**
    * don't know TODO
    * @param title litmus test name + rf candidates name
    * @param p pipeline
    * @param events list of events in litmus text
    * @param rf reads from edges
    * @return verification result (legal execution or forbidden case)
    */
  def GraphsToVerifyExecution(title: String, p: Pipeline, events: List[Event], rf: List[(Eiid, Eiid)])
  : (Boolean, Int, List[(String, String)]) = {
    print(PrintRFList(rf))
    val (b, i, lv) = VerifyExecution(p, events, rf)
    (b, i, lv.map(GraphOfExecutionVerificationResult(title, p, _)))
  }
}
