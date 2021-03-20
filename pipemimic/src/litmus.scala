package pipemimic

import CartesianUtils.CartesianProduct
import Execution.GraphsToVerifyExecution
import Stages.Pipeline

import scala.annotation.tailrec

object LitmusTestResult extends Enumeration {
  val Forbidden, Permitted = Value
}

object Litmus {
  /**
    * all permutations of elements in l
    * FIXME used in nowhere
    * @param l provide elements
    * @tparam A type of elements in l
    * @return all possible permutations
    */
  def Permutations[A](l: List[A]): List[List[A]] = {
    /**
      * insert a into different places in l2, then append l2' to l1
      * @param a pivot insert into l2
      * @param l1 prefix list
      * @param l2 suffix list
      * @tparam B type of element in l1 and l2
      * @return l1 ::: (permutations of a and l2)
      */
    def helper[B](a: B, l1: List[B], l2: List[B]): List[List[B]] = {
      l2 match {
        /* place a in front of l2 || insert into tail of l2 */
        case head :: next => (l1 ::: (a :: l2)) :: helper(a, l1.appended(head), next)
        case Nil => List(l1.appended(a))
      }
    }

    l match {
      case Nil => Nil
      case List(h) => List(List(h))
      case head :: next => Permutations(next).flatMap(helper(head, Nil, _))
    }
  }

  /**
    * Given a read event r, find matching write events in l
    * @param r a read event
    * @param l list of events
    * @return (Some(e), r): r may read value written by e; (None, r): r may read initial value
    */
  def MatchingWrites(r: Event, l: List[Event]): List[(Option[Event], Event)] = {
    l match {
      case w :: t => (r.action, w.action) match { /* location and value in write/read event matches */
        case (Access(dr, lr, vr), Access(dw, lw, vw)) => (dr, dw, lr == lw, vr == vw) match {
          case (Direction.R, Direction.W, true, true) => (Some(w), r) :: MatchingWrites(r, t)
          case _ => MatchingWrites(r, t)
        }
      }
      case Nil => r.action match {
        // FIXME why value must be 0 ? what's the meaning of read's value ?
        case Access(Direction.R, _, 0) => List((None, r))
        case _ => Nil
      }
    }
  }

  /**
    * currently, call this method would return legal execution or list of forbidden cases on certain litmus test
    * TODO change this into a class and provide litmus test parser instead of define test in source code
    * @param name name of litmus test
    * @param expected expected output (Permitted/Forbidden) of litmus test on certain MCM
    * @param p pipeline
    * @param events list of event
    * @return legal execution or forbidden case
    */
  def LitmusTest(name: String, expected: LitmusTestResult.Value, p: Pipeline, events: List[Event])
  : List[(String, String)] = {
    /**
      * Given list of rf pairs, return list of event id pairs
      * @param l rf pairs (output of MatchingWrites)
      * @return edges (event id pairs)
      */
    def GetEiidPair(l: List[(Option[Event], Event)]): List[(Eiid, Eiid)] = {
      l match {
        case head :: next => head match {
          case (Some(w), r) => (w.eiid, r.eiid) :: GetEiidPair(next)
          case _ => GetEiidPair(next)
        }
        case Nil => Nil
      }
    }

    /**
      * Given a litmus test and a particular RF candidate, generate the result for that litmus test
      * @param name name of litmus test
      * @param expected expected output (Permitted/Forbidden) of litmus test on certain MCM
      * @param p pipeline
      * @param events list of event
      * @param rf reads from edges
      * @return legal execution or forbidden case
      */
    def VerifyOneCaseForOneTest(name: String, expected: LitmusTestResult.Value, p: Pipeline, events: List[Event],
                                rf: List[(Option[Event], Event)]): (Boolean, Int, List[(String, String)]) = {
      def LitmusTestCandidateName(r: LitmusTestResult.Value, rf: List[(Eiid, Eiid)]): String = {
        def LitmusTestResultString(r: LitmusTestResult.Value) = {
          "(exp: " + { if (r == LitmusTestResult.Forbidden) "Forbidden" else "Permitted" } + ")"
        }

        def LitmusTestCandidateRF(rf: (Eiid, Eiid)) = s"(${rf._1}-rf->${rf._2})" /* reads from edge */
        def LitmusTestCandidateWS(w: Eiid) = s" $w" /* write serialization edge */

        s"${LitmusTestResultString(r)} RF: ${rf.map(LitmusTestCandidateRF).mkString}" // FIXME why not ws here?
      }

      val _rf = GetEiidPair(rf)
      val _name = name + LitmusTestCandidateName(expected, _rf) /* test name + candidate name */
      GraphsToVerifyExecution(_name, p, events, _rf)
    }

    /**
      * Given a litmus test and a list of RF edge candidates, either find a legal execution, or return the list of all
      * forbidden cases.
      *
      * @param name name of litmus test
      * @param expected expected output (Permitted/Forbidden) of litmus test on certain MCM
      * @param p pipeline
      * @param events list of event
      * @param rfs rf edge candidates
      * @param lr list of verification results
      * @param n number of FIXME what?
      * @return legal execution or forbidden cases
      */
    @tailrec
    def VerifyAllCasesForOneTest(name: String, expected: LitmusTestResult.Value, p: Pipeline,
                                 events: List[Event], rfs: List[List[(Option[Event], Event)]],
                                 lr: List[(String, String)], n: Int): (Boolean, Int, List[(String, String)]) = {
      rfs match {
        case head :: next => VerifyOneCaseForOneTest(name, expected, p, events, head) /* check one case in rf list */ match {
          case (true, _n, r) => (true, n + _n, r) /* find a legal execution */
          case (false, _n, r) => VerifyAllCasesForOneTest(name, expected, p, events, next, lr ::: r, n + _n) /* execute
          following candidates while append r (GraphOfExecutionVerificationResult) to lr and add _n to n */
        }
        case Nil => (false, n, lr)
      }
    }

    /**
      * verify given litmus test
      * @param name name of litmus test
      * @param expected expected output (Permitted/Forbidden) of litmus test
      * @param p pipeline
      * @param events list of event
      * @return legal execution or forbidden cases
      */
    def GenerateCandidatesThenVerify(name: String, expected: LitmusTestResult.Value, p: Pipeline, events: List[Event]): (Boolean, Int, List[(String, String)]) = {
      def StringOfRFCandidates(rf: List[List[(Option[Event], Event)]]): String = {
        def stringHelper(rf: List[(Option[Event], Event)]): String = {
          rf match {
            case head :: next => { head match {
              case (Some(e1), e2) => s" ${e1.eiid}-rf->${e2.eiid}"
              case (None, e2) => s"i-rf->${e2.eiid} "
            }} + stringHelper(next)
            case Nil => ""
          }
        }
        rf match {
          case Nil => "\n"
          case head :: next => s"RF Candidates: (${stringHelper(head)} ) ${StringOfRFCandidates(next)}"
        }
      }

      /* separate read and write events */
      val (writes, reads) = events.partition(_.isWrite)
      /* for each read event, find its matching write events in all write events */
      val rfCandidates = reads.map(r => MatchingWrites(r, _)).map(f => f(writes))
      println(StringOfRFCandidates(rfCandidates))
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
      val rf = CartesianProduct(rfCandidates)
      println(StringOfRFCandidates(rf))
      VerifyAllCasesForOneTest(name, expected, p, events, rf, Nil, 0)
    }

    print(s"\nLitmus Test,${p.pipeName},$name\n")
    val ms = new TinyTimer("LitmusTest")
    ms.reset()
    val (b, n, r) = GenerateCandidatesThenVerify(name, expected, p, events)
    println(ms)
    print(s"\nLitmus Test Result,$n,$b\n\n")
    r
  }

  /* add one litmus test for testing */

  def amd1(p: Pipeline): List[(String, String)] = {
    LitmusTest("iwp2.1/amd1", LitmusTestResult.Forbidden, p, List(
      Event(0, Iiid(0, 0), Access(Direction.W, 0, 1)),
      Event(1, Iiid(0, 1), Access(Direction.W, 1, 1)),
      Event(2, Iiid(1, 0), Access(Direction.R, 1, 1)),
      Event(3, Iiid(1, 1), Access(Direction.R, 0, 0))
    ))
  }

  val AllLitmusTests: Seq[Pipeline => List[(String, String)]] = amd1 :: Nil
}