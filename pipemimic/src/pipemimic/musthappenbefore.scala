package pipemimic

import scala.annotation.tailrec

import topology.VerifyMustHappenBeforeInGraph

sealed abstract class MHBResult

case class Unverified(g: List[(Int, Int, String)], a: Int, b: Int) extends MHBResult
case class MustHappenBefore(g: List[(Int, Int, String)], l: List[Int]) extends MHBResult
case class Cyclic(g: List[(Int, Int, String)], l: List[Int]) extends MHBResult

object MustHappenBefore {

  def TreeAcyclicInSomeGraph(g: GraphTree[Int]): (List[(String, MHBResult)], Boolean, Int) = {

    @tailrec
    def helper(lg: List[(String, List[(Int, Int, String)])], lr: List[(String, MHBResult)]): (List[(String, MHBResult)], Boolean) = {
      
      def tuple(g: List[(Int, Int, String)]): (MHBResult, Boolean) = {
        val r = g.existsPath(0, 0)
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