//package pipemimic
//
//import pipemimic.Execution.GraphsToVerifyExecution
//import results.LitmusResult
//
//import scala.collection.mutable
//import scala.collection.mutable.ArrayBuffer
//import scala.util.control.Breaks
//
//class LitmusTest(name: String, expected: LitmusTestExpectation.Value, pipeline: Pipeline, events: List[Event]) {
//
//  /* Generate candidates then verify */
//
//  /* separate read and write events */
//  private val (writes, reads) = events.partition(_.isWrite)
//
//  /**
//    * Given a read event r, find matching write events in l
//    * @param r a read event
//    * @param l list of events
//    * @return (Some(e), r): r may read value written by e; (None, r): r may read initial value
//    */
//  private def MatchingWrites(r: Event, l: List[Event]): List[(Option[Event], Event)] = {
//    l match {
//      case w :: t => (r.action, w.action) match { /* location and value in write/read event matches */
//        case (Access(dr, lr, vr), Access(dw, lw, vw)) => (dr, dw, lr == lw, vr == vw) match {
//          case (Direction.R, Direction.W, true, true) => (Some(w), r) :: MatchingWrites(r, t)
//          case _ => MatchingWrites(r, t)
//        }
//      }
//      case Nil => r.action match {
//        case Access(Direction.R, _, 0) => List((None, r))
//        case _ => Nil
//      }
//    }
//  }
//
//  private def StringOfRFCandidates(rf: List[List[(Option[Event], Event)]]): String = {
//    def stringHelper(rf: List[(Option[Event], Event)]): String = {
//      rf match {
//        case head :: next => { head match {
//          case (Some(e1), e2) => s" ${e1.eiid}-rf->${e2.eiid}"
//          case (None, e2) => s"i-rf->${e2.eiid} "
//        }} + stringHelper(next)
//        case Nil => ""
//      }
//    }
//    rf match {
//      case Nil => "\n"
//      case head :: next => s"RF Candidates: (${stringHelper(head)} ) ${StringOfRFCandidates(next)}"
//    }
//  }
//
//  /* for each read event, find its matching write events in all write events */
//  private val rfCandidates = reads.map(r => MatchingWrites(r, _)).map(f => f(writes))
//  println(StringOfRFCandidates(rfCandidates))
//  /**
//    * rfCandidates:
//    * r1: (Some w, r1), (None, r1) ... => r1 may reads from w, or initial value, or ...
//    * r2: (None, r2) ...
//    * ...
//    * rn: (Some w, rn), (Some w, rn) ...
//    *
//    * rf = r1 * r2 * ... * rn (* = cartesian product)
//    * thus, rf includes all possible cases of combination of reads from edges
//    */
//  private val rf = CartesianProduct(rfCandidates)
//  println(StringOfRFCandidates(rf))
//
//  /* Verify all cases for this test */
//
//  var casesCnt = 0
//  val unobservedCases: ArrayBuffer[LitmusResult] = ArrayBuffer.empty[LitmusResult]
//  var observable = false
//
//  private def candidateName(r: LitmusTestExpectation.Value, rf: List[(Eiid, Eiid)]): String = {
//    s"(exp: ${ if (expected == LitmusTestExpectation.Forbidden) "Forbidden" else "Permitted" }) RF: " +
//      rf.map { case (w, r) => s"($w-rf->$r)" }.mkString
//  }
//
//  private val findObservable = new Breaks
//
//  findObservable.breakable {
//    for (sourceLocationForEachReadEvent <- rf) {
//      /* check one case in rf list */
//      val eiidPairs = sourceLocationForEachReadEvent.filter(_._1.isDefined) map {
//        case (Some(w), r) => (w.eiid, r.eiid)
//      }
//      val caseName = name + candidateName(expected, eiidPairs)
//      /* find out if this case is observable */
//      val result = pipeline.executeLitmusTest(events, eiidPairs)
//
//      if (result.observable) {
//        /* return corresponding uhb graph */
//        observable = true
//        findObservable.break()
//      } else {
//        unobservedCases += result
//        casesCnt += 1
//      }
//    }
//  }
//}
