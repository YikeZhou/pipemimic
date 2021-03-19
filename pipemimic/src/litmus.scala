package pipemimic

import CartesianUtils.CartesianProduct
import Execution.GraphsToVerifyExecution

object LitmusTestResult extends Enumeration {
  val Forbidden, Permitted = Value
}

object Litmus {

  def Permutations[A](l: List[A]): List[List[A]] = {
    def helper[A](a: A, l1: List[A], l2: List[A]): List[List[A]] = {
      l2 match {
        case head :: next => (l1 ::: (a :: l2)) :: helper(a, l1.appended(head), next)
        case Nil => List(l1.appended(a))
      }
    }

    l match {
      case Nil => Nil
      case List(h) => List(List(h))
      case head :: next => Permutations(next).map(helper(head, Nil, _)).flatten
    }
  }

  def MatchingWrites(r: Event, l: List[Event]): List[(Option[Event], Event)] = {
    l match {
      case w :: t => (r.action, w.action) match {
        case (Access(dr, lr, vr), Access(dw, lw, vw)) => (dr, dw, lr == lw, vr == vw) match {
          case (Direction.R, Direction.W, true, true) => (Some(w), r) :: MatchingWrites(r, t)
          case _ => MatchingWrites(r, t)
        }
      }
      case Nil => r.action match {
        case Access(Direction.R, _, 0) => List((None, r))
        case _ => Nil
      }
    }
  }

  def LitmusTestCandidateName(r: LitmusTestResult.Value, rf: List[(Eiid, Eiid)]): String = {
    def LitmusTestResultString(r: LitmusTestResult.Value) = {
      "(exp: " + { if (r == LitmusTestResult.Forbidden) "Forbidden" else "Permitted" } + ")"
    }

    def LitmusTestCandidateRF(rf: (Eiid, Eiid)) = s"(${rf._1}-rf->${rf._2})"
    def LitmusTestCandidateWS(w: Eiid) = s" $w"

    s"${LitmusTestResultString(r)} RF: ${rf.map(LitmusTestCandidateRF(_)).mkString}"
  }

  def LitmusTest(name: String, expected: LitmusTestResult.Value, p: Stages.Pipeline, events: List[Event]): List[(String, String)] = {

    def ___LitmusTest(l: List[(Option[Event], Event)]): List[(Eiid, Eiid)] = {
      l match {
        case head :: next => head match {
          case (Some(w), r) => (w.eiid, r.eiid) :: ___LitmusTest(next)
          case _ => ___LitmusTest(next)
        }
        case Nil => Nil
      }
    }
    
    def __LitmusTest(name: String, expected: LitmusTestResult.Value, p: Stages.Pipeline, events: List[Event], rf: List[(Option[Event], Event)]): (Boolean, Int, List[(String, String)]) = {
      val _rf = ___LitmusTest(rf)
      val _name = name + LitmusTestCandidateName(expected, _rf)
      GraphsToVerifyExecution(_name, p, events, _rf)
    }
    
    def _LitmusTest(name: String, expected: LitmusTestResult.Value, p: Stages.Pipeline, events: List[Event], rf: List[List[(Option[Event], Event)]], lr: List[(String, String)], n: Int): (Boolean, Int, List[(String, String)]) = {
      rf match {
        case head :: next => ???
        case Nil => (false, n, lr)
      }
    }
    
    def helper(name: String, expected: LitmusTestResult.Value, p: Stages.Pipeline, events: List[Event]): (Boolean, Int, List[(String, String)]) = {
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

      val (writes, reads) = events.partition(_.isWrite)
      val rfCandidates = reads.map(r => MatchingWrites(r, _)).map(f => f(writes))
      println(StringOfRFCandidates(rfCandidates))
      val rf = CartesianProduct(rfCandidates)
      println(StringOfRFCandidates(rf))
      _LitmusTest(name, expected, p, events, rf, Nil, 0)
    }

    print(s"\nLitmus Test,${p.pipeName},$name\n")
    val ms = new TinyTimer("LitmusTest")
    ms.reset
    val (b, n, r) = helper(name, expected, p, events)
    println(ms)
    print(s"\nLitmus Test Result,$n,$b\n\n")
    r
  }
}