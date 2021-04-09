package pipemimic.execution

import pipemimic._
import pipemimic.statistics.DotGraph

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.control.Breaks._

class LitmusTest(name: String, expected: LitmusTestResult.Value, events: List[Event]) {
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

  /** cases calculated when verifying (stop when find first observable cases) */
  private var casesCnt = 0

  /** cases ruled out because of cyclic graph */
  private val unobservedCases: ListBuffer[DotGraph] = ListBuffer.empty[DotGraph]

  private val observedCases: ListBuffer[DotGraph] = ListBuffer.empty[DotGraph]

  /** when coming across first observable case, set observable to true */
  private var observable = false

  breakable {
    for (sourceLocationForEachReadEvent <- rf) {
      /* check one case in rf list */

      /* change rf events pair into eiid pair */
      val eiidPairs = sourceLocationForEachReadEvent.filter(_._1.isDefined) map {
        case (Some(w), r) => (w.eiid, r.eiid)
      }

      /* generate a readable name for each rf relationship */
      val caseName = name /* litmus test name */ + candidateName(expected, eiidPairs)

      /* find out if this case is observable */
      // TODO call GraphsToVerifyExecution in execution.scala

//      if (result.observable) {
//        /* find a legal execution */
//        observable = true
//        observedCases += result.dotgraph
//        break
//      } else {
//        unobservedCases += result.dotgraph
//        casesCnt += 1
//      }
    }
  }

}