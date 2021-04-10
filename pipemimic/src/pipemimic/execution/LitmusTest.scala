package pipemimic.execution

import pipemimic._
import pipemimic.statistics.DotGraph
import pipemimic.topology.VerifyMustHappenBeforeInGraph

import scala.collection.mutable.ListBuffer

class LitmusTest(name: String, expected: LitmusTestExpectedResult.Value, events: List[Event])
  extends ScenariosForEvents /* generate scenarios */
  with WriteSerializationEdge /* ws edges */
  with ReadsFromEdge /* rf edges */
  with FromReadEdge /* fr edges */
{
  println(s"Litmus Test <$name>")

  /* Generate reads-from relation candidates */

  /* separate read and write events */
  private val writeEvents = events.filter(_.isWrite)
  private val readEvents = events.filter(_.isRead)

  /**
    * Given a read event r, find matching write events in l
    * @param r a read event
    * @param l list of events
    * @return (Some(e), r): r may read value written by e; (None, r): r may read initial value
    */
  private def matchingWrites(r: Event, l: List[Event]): List[(Option[Event], Event)] = {
    val matched = ListBuffer.empty[Option[Event]]

    /* check if r can read from initial value (0) */
    r.action match {
      case Access(Direction.R, address, 0) => matched += None
      case _ =>
    }

    /* iterate through l */
    l foreach { w =>
      (r.action, w.action) match {
        /* location and value in write/read event matches */
        case (Access(dr, lr, vr), Access(dw, lw, vw)) => (dr, dw, lr == lw, vr == vw) match {
          case (Direction.R, Direction.W, true, true) => matched += Some(w)
          case _ =>
        }
        case _ =>
      }
    }

    matched.toList.map((_, r))
  }

  /* for each read event, find its matching write events in all write events */
  private val rfCandidates = readEvents.map(r => matchingWrites(r, _)).map(f => f(writeEvents))

  println("Matched read and write events: ")
  for ((sameRead, index) <- rfCandidates.zipWithIndex) {
    println(s"Read Event: ${readEvents(index)}")
    println(sameRead.mkString(" "))
  }
  /**
    * rfCandidates:
    * r1: (Some w, r1), (None, r1) ... => r1 may reads from w, or initial value, or ...
    * r2: (None, r2) ...
    * ...
    * rn: (Some w, rn), (Some w, rn) ...
    *
    * rf = r1 * r2 * ... * rn (* = cartesian product)
    * thus, rf includes all possible cases of combination of reads from edges
    */
  private val rf = CartesianProduct(rfCandidates)

  println("All combinations of reads-from relationship in current litmus test")
  for ((possibility, index) <- rf.zipWithIndex) {
    println(s"Case $index")
    println(possibility.mkString(" "))
  }

  /* Verify all cases for this test */

  def getResults(pipeline: Pipeline): LitmusTestResult = {
    /** cases calculated when verifying (stop when find first observable cases) */
    var casesCnt = 0

    /** cases ruled out because of cyclic graph */
    val unobservedCases: ListBuffer[DotGraph] = ListBuffer.empty[DotGraph]

    val observedCases: ListBuffer[DotGraph] = ListBuffer.empty[DotGraph]

    /** when coming across first observable case, set observable to true */
    var observable = false

    def getEventName: Int => String = x => GlobalEventString(pipeline, ungeid(pipeline, x))

    for (sourceLocationForEachReadEvent <- rf) {
      /* check one case in rf list */
      val (readsFromWriteValue, readsFromInitValue) = sourceLocationForEachReadEvent.partition(_._1.isDefined)

      /* change rf events pair into eiid pair */
      val eiidPairs = readsFromWriteValue map {
        case (Some(w), r) => (w.eiid, r.eiid)
        case _ => sys.error("LitmusTest: fatal error in finding reads from write value")
      }

      /* generate a readable name for each rf relationship */
      val caseName = name /* litmus test name */ + candidateName(expected, eiidPairs)

      /* find out if this case is observable */

      /** generate all combinations of events' path */
      val scenarios = getScenarios(pipeline, events)
      println(s"Found ${scenarios.length} scenarios")

      for ((scTitle, scenario) <- scenarios) {
        /* verify a single scenario for current rf candidate */
        val staticEdges = StaticEdges(scTitle, pipeline, scenario)

        val ws = wsEdges(scenario)
        val rf = rfEdges(ws, eiidPairs, scenario, pipeline)
        val fr = frEdges(readsFromInitValue.map(_._2), scenario, pipeline)

        val observedEdges = GraphTreeAnd(List(ws, rf, fr))

        /* global event -> int value */
        val rawGraphs = getid(pipeline, GraphTreeAnd(List(staticEdges, observedEdges))).flatten

        /* check if graph contains cycle */
        println(s"Found ${rawGraphs.length} graphs in current rf candidate and scenario")
        for (graph <- rawGraphs) {
          casesCnt += 1
          /* graph name: caseName + scTitle + graphTitle? */
          val (graphTitle, graphEdges) = graph
          graphEdges.existsCycle match {
            case Some(cycle) => /* ruled out */
              /* generate dot graph */
              unobservedCases += new DotGraph(s"Forbidden: $caseName$scTitle$graphTitle", graphEdges,
                ungeid(pipeline, _), getEventName, cycle, cycle.pairConsecutive, pipeline.stages.length)
            case None =>
              observable = true
              /* generate dot graph */
              observedCases += new DotGraph(s"Permitted: $caseName$scTitle$graphTitle", graphEdges,
                ungeid(pipeline, _), getEventName, Nil, Nil, pipeline.stages.length)
              return LitmusTestResult(observable, observedCases.toList, unobservedCases.toList, casesCnt)
          }
        }
      }
    }

    LitmusTestResult(observable, observedCases.toList, unobservedCases.toList, casesCnt)
  }

}
