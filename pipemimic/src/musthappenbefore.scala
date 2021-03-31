package pipemimic

import TopologicalSort.TopSort
import Adjacency.{Dijkstra, PathBacktrace}

import scala.annotation.tailrec

abstract class MHBResult

case class Unverified(g: List[(Int, Int, String)], a: Int, b: Int) extends MHBResult
case class MustHappenBefore(g: List[(Int, Int, String)], l: List[Int]) extends MHBResult
case class Cyclic(g: List[(Int, Int, String)], l: List[Int]) extends MHBResult

object MustHappenBefore {
   private def VerifyMustHappenBeforeInGraph(g: List[(Int, Int, String)], sd: (Int, Int)): MHBResult = {
    val (src, dst) = sd
    val _g = g.map(node => (node._1, node._2))
    TopSort(_g) match {
      case TotalOrdering(_) =>
        val (reachable, prev) = Dijkstra(_g, src)
        if (reachable.contains(dst)) {
          MustHappenBefore(g, PathBacktrace(_g, prev, src, dst))
        } else {
          Unverified(g, src, dst)
        }
      case CycleFound(p) => Cyclic(g, p)
    }
  }

  def TreeMustHappenBeforeInAllGraphs(g: GraphTree[Int], v: GraphTree[Int]): List[(String, MHBResult)] = {

    @tailrec
    def boolPair(g: List[(Int, Int, String)], lsd: List[(Int, Int, String)], lr: List[MHBResult], b: Boolean): (Boolean, List[MHBResult]) = {
      lsd match {
        case head :: next =>
          val r = VerifyMustHappenBeforeInGraph(g, (head._1, head._2))
          r match {
            case Unverified(_, _, _) => boolPair(g, next, r :: lr, b = false)
            case MustHappenBefore(_, _) => boolPair(g, next, r :: lr, b)
            case Cyclic(_, _) => boolPair(g, next, r :: lr, b)
          }
        case Nil => (b, lr)
      }
    }

    @tailrec
    def stringPair(n: String, g: List[(Int, Int, String)], lv: List[(String, List[(Int, Int, String)])], lr: List[(String, MHBResult)]): List[(String, MHBResult)] = {
      lv match {
        case (hn, hv) :: next =>
          val (b, r) = boolPair(g, hv, Nil, b = true) /* lv: result of DNFOfTree */
          val _r = r.map((s"$n:$hn", _))
          /* It seems that edges in lv should appear in every graph in g,
          * but here only check for existence in g */
          if (b) /* success */ _r else /* check next one */ stringPair(n, g, next, lr ::: _r)
        case Nil => lr
      }
    }

    def helper(lg: List[(String, List[(Int, Int, String)])], lv: List[(String, List[(Int, Int, String)])]): List[(String, MHBResult)] = {
      lg match {
        case (hs, hg) :: next => stringPair(hs, hg, lv, Nil) ::: helper(next, lv)
        case Nil => Nil
      }
    }

    helper(g.flatten, v.flatten)
  }

  def TreeAcyclicInSomeGraph(g: GraphTree[Int]): (List[(String, MHBResult)], Boolean, Int) = {

    @tailrec
    def helper(lg: List[(String, List[(Int, Int, String)])], lr: List[(String, MHBResult)]): (List[(String, MHBResult)], Boolean) = {
      
      def tuple(g: List[(Int, Int, String)]): (MHBResult, Boolean) = {
        val r = VerifyMustHappenBeforeInGraph(g, (0, 0))
        r match {
          case Cyclic(_, _) => (r, false)
          case _ => (r, true) /* can happen */
        }
      }

      lg match {
        case (hs, hg) :: next =>
          val (r, b) = tuple(hg)
          if (b) (List((hs, r)), true) else helper(next, (hs, r) :: lr)
        case Nil => (lr, false)
      }
    }

    val d = g.flatten
    val r = helper(d, Nil)
    (r._1, r._2, d.length)
  }
}