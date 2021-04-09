package pipemimic

import pipemimic.Stages._
import pipemimic.MustHappenBefore.TreeAcyclicInSomeGraph
import pipemimic.topology.PathFinder
import Dot.DotGraph
import organization.Interleaving

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Execution extends GlobalGraphID {

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
    val reachable = PathFinder(g3, geid(p, src)).getAllReachableNodes
    reachable.map(ungeid(p, _))
  }

  def nthEventInScenarioIsWrite(s: Scenario, ge: (Int, Eiid)): Boolean = {
    s.lift(ge._2) match {
      case Some(po) => po.evt.isWrite
      case None => false
    }
  }

  def VertexHasSameAddress(s: Scenario, l: Option[Location], e: GlobalEvent): Boolean = {
    s.lift(e._2) match {
      case Some(po) => po.evt.addr == l
      case None => false
    }
  }

  def ReachableVerticesAtSameLocation(p: Pipeline, s: Scenario, src: GlobalEvent,
                                      g: List[(GlobalEvent, GlobalEvent, String)]): List[Eiid] = {
    s.lift(src._2) match {
      case Some(po) => ReachableVertices(p, src, g)
                        .filter(VertexHasSameAddress(s, po.evt.addr, _))
                        .filter(nthEventInScenarioIsWrite(s, _))
                        .map(_._2)
                        .toSet
                        .filterNot(_ == src._2)
                        .toList
      case None => Nil
    }
  }

  def WritesToSameLocation(l: Option[Location], s: Scenario): List[PathOption] = {
    s.filter(x => if (x.evt.dirn.contains(Direction.R)) false else x.evt.addr == l)
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
        case (false, Some(Direction.R)) => /* head: read event from init */ head.eiid :: ReadsFromInitial(next,
          rfFromWrite)
        case _ => ReadsFromInitial(next, rfFromWrite)
      }
      case Nil => Nil
    }
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
      case head :: next => (head.evt.dirn, head.evt.addr) match {
        case (Some(Direction.W), Some(l)) => ScenarioExecutionEdges_WS_SortByLoc(next).appendToNth(l, head)
        case _ => ScenarioExecutionEdges_WS_SortByLoc(next)
      }
      case Nil => Nil
    }
  }

  /* Verification */

  def VerifyExecutionScenario(p: Pipeline, scenarioWithTitle: (String, Scenario), rf: List[(Eiid, Eiid)])
  : (List[(String, MHBResult)], Boolean, Int) = {
    val (title, scenario) = scenarioWithTitle
    val staticEdges = ScenarioEdges(title, p, scenario)

    val observedEdges = {
      /* calculate all of the WS edges */
      val wsEdges: GraphTree[GlobalEvent] = {
        val edgesPerInterleaving = {
          val sortedByLocation = ScenarioExecutionEdges_WS_SortByLoc(scenario)
          val writeEventsSortByLocThenCore = sortedByLocation.map(ScenarioExecutionEdges_WS_SortByCore)
          /* element in wsCandidateForEachLocation : all possibilities of write serialization for location 0~n */
          val wsCandidateForEachLocation = writeEventsSortByLocThenCore.map(Interleaving[GlobalEvent](_: _*))
          /* for each possible case, generate a edge list */
          /* when there is only 1 write op at location, no ws edge will be generated */
          wsCandidateForEachLocation.map(_.map(_.pairConsecutive("WS")))
        }
        /* turn list of global edges into a graph tree */
        val rawGraph = GraphTreeAnd(edgesPerInterleaving.map { wsCandidatesAtLocation =>
          val wsPossibilities = wsCandidatesAtLocation map { candidate =>
            /* one possible write serialization */
            GraphTreeLeaf(ExecutionEdgeLabel("ws", candidate), candidate)
          }
          println(s"WS @ location: ${wsPossibilities.length} candidates\n")
          GraphTreeOr(wsPossibilities)
        })
        rawGraph // FIXME
      }


      val rfAndFrEdges: GraphTree[GlobalEvent] = {
        /* add rf edges */
        val readsFromEdges = ListBuffer.empty[GraphTree[GlobalEvent]]
        for (readsFrom <- rf) {
          readsFromEdges += {
            val (write, read) = readsFrom /* eiid of write and read events */
            (pathOfEvent(scenario, write), pathOfEvent(scenario, read)) match {
              case (Some(pathOfWrite), Some(pathOfRead)) =>
                /* given path of read event and write event, return rf edges and fr edges if exists */

                /* one rf pair may correspond multiple edges */
                val rfPossibilities = {
                  val performLocationPairs = RFPerformPairs(pathOfWrite, pathOfRead)
                  performLocationPairs map { case (writePerformLocation, readPerformLocation) =>
                    val writeGlobalEvent = (writePerformLocation, write)
                    val readGlobalEvent = (readPerformLocation, read)
                    val e = (writeGlobalEvent, readGlobalEvent, "RF") /* one rf edge */
                    val le = List(e)
                    /* output a edge */
                    def PrintPossibility: GraphTree[GlobalEvent] => String =
                      t => t.toString(GlobalEventString(p, _)) + '\n'
                    println(PrintPossibility(GraphTreeLeaf("rf_uhb", le)))
                    /* check if exists fr edge */
                    val fr = {
                      /* given a RF edge (w, r), for all vertices w' in uhb graph such that (w, w') is an edge
                      between events at the same location, add the fr edge (r, w') */
                      val allWSEdges = wsEdges.flatten
                      val frEdges = allWSEdges map { case (wsName, wsCandidate) =>
                        val reachableWrite = ReachableVerticesAtSameLocation(p, scenario, writeGlobalEvent, wsCandidate)
                        val pathOfReachableWrite = reachableWrite.map(pathOfEvent(scenario, _))
                          .filter(_.isDefined).map{ case Some(p) => p }
                        (
                          wsName,
                          pathOfReachableWrite.flatMap(w => FRfwPerformPairs(pathOfRead, w).map { case (rLoc, wLoc) =>
                            ((rLoc, read), (wLoc, w.evt.eiid), "FRfw") /* fr edge */
                          })
                        )
                      }
                      GraphTree(frEdges)
                    }
                    GraphTreeAnd(List(fr, GraphTreeLeaf(ExecutionEdgeLabel("rf", le), le)))
                  }
                }
                println(s"Source path ${pathOfWrite.optionName}, Dest path ${pathOfRead.optionName}\n")
                println(s"Architectural RF edge: ${rfPossibilities.length} uhb candidates\n")
                GraphTreeOr(rfPossibilities)

              case _ => sys.error("ScenarioExecutionEdges_RF_fromwrite: event not in scenario")
            }
          }
        }
        /* add fr edges */
        val fromReadEdges = ListBuffer.empty[GraphTree[GlobalEvent]]
        val rfInitialReads = ReadsFromInitial(scenario.map(_.evt), rf) // FIXME: add initial values in litmus tests
        for (readsFromInitValue <- rfInitialReads) {
          /* given event readsFromInitValue, return corresponding fr edge */
          pathOfEvent(scenario, readsFromInitValue) match {
            case Some(pathOfReadFromInitial) =>
              /* find write event to same location */
              val writeEventToSameAddress = WritesToSameLocation(pathOfReadFromInitial.evt.addr, scenario)
              writeEventToSameAddress foreach { pathOfWrite => /* readsFromInit -fr-> write */
                FRInitialPerformPairs(pathOfReadFromInitial, pathOfWrite) map { case (rLoc, wLoc) =>
                  /* add all fr edge into list buffer fromReadEdges */
                  val readGlobalEvent = (rLoc, pathOfReadFromInitial.evt.eiid)
                  val writeGlobalEvent = (wLoc, pathOfWrite.evt.eiid)
                  val e = (readGlobalEvent, writeGlobalEvent, "FRi")
                  val le = List(e)
                  def PrintPossibility: GraphTree[GlobalEvent] => String =
                    t => t.toString(GlobalEventString(p, _)) + '\n'
                  println(PrintPossibility(GraphTreeLeaf("fr_uhb", le)))
                  fromReadEdges += GraphTreeLeaf(ExecutionEdgeLabel("fr", le), le)
                }
              }
            case None =>
              sys.error("ScenarioExecutionEdges_FR_initial: event is not actually in scenario")
          }
        }

        GraphTreeAnd(readsFromEdges.addAll(fromReadEdges).toList)
      }

      GraphTreeAnd(List(wsEdges, rfAndFrEdges))
    }

    TreeAcyclicInSomeGraph(/* global event -> int value */getid(p, GraphTreeAnd(List(staticEdges, observedEdges))))
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
    val scenarios = ScenariosForEvents(p, events) /* all combinations of events' path */
    println(s"Found ${scenarios.length} scenarios\n")

    var checkedCases = 0
    val results = ListBuffer.empty[(String, MHBResult)]

    for (scenario <- scenarios) {
      val (mhb, observable, count) = VerifyExecutionScenario(p, scenario, rf)
      if (observable) {
        return (true, count + checkedCases, mhb)
      } else {
        checkedCases += count
        results ++= mhb
      }
    }
    (false, checkedCases, results.toList)
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
