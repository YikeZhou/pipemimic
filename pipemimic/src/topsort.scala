package pipemimic

import Adjacency.{AdjacencyList, AdjacencyListFromEdges, FindPathAdj}

import scala.annotation.tailrec

abstract class TopSortResult

case class TotalOrdering(l: List[Int]) extends TopSortResult
case class CycleFound(l: List[Int]) extends TopSortResult

object Marking extends Enumeration {
  val Unmarked, MarkedTemp, Marked = Value
}

/* Topological Sort */
object TopologicalSort {
  /**
    * Given an adjacency list, return a list of booleans such that
    * the nth element is true if there is at least one edge
    * connected to node n.
    *
    * @param g adjacency list
    * @return list of booleans
    */
  def EdgesWithConnections(g: AdjacencyList): List[Boolean] = {
    /* The destination of each edge is connected */
    def checkDestination(l: List[Int], b: List[Boolean]): List[Boolean] = {
      l match {
        case head :: next => checkDestination(next, b).replaceNth(head, true, false)
        case Nil => b
      }
    }
    /* Include the source and dest of each edge as connected */
    @tailrec
    def includeSource(g: AdjacencyList, b: List[Boolean], n: Int): List[Boolean] = {
      require(n >= 0)

      g match {
        case head :: next =>
          val _b = head match {
            case _ :: _ => /* outgoing edge from this node - edge is connected */ b.replaceNth(n, true, false)
            case Nil => /* no outgoing edge from this node */ b
          }
          includeSource(next, checkDestination(head, _b), n + 1)
        case Nil => b
      }
    }

    includeSource(g, Nil, 0)
  }

  def TopSortVisit(unroll: Int, g: List[List[Int]], omr: (List[Marking.Value], TopSortResult), n: Int): (List[Marking.Value], TopSortResult) = {
    require(unroll >= 0 && n >= 0)

    omr match {
      case (m, TotalOrdering(r)) => (unroll, m.nthDefault(n, Marking.Unmarked)) match {
        case (u, Marking.Unmarked) if u > 0 =>
          val _m = m.replaceNth(n, Marking.MarkedTemp, Marking.Unmarked)
          val adjOfN = g.nthDefault(n, Nil)
          adjOfN.foldLeft[(List[Marking.Value], TopSortResult)]((_m, TotalOrdering(r)))((tuple, i) => TopSortVisit(u - 1, g, tuple, i)) match {
            case (__m, TotalOrdering(__r)) => (__m.replaceNth(n, Marking.Marked, Marking.Unmarked), TotalOrdering
            (List(n) ::: __r))
            case t => t
          }
        case (u, Marking.Marked) if u > 0 => omr
        case (0, _) => (m, CycleFound(List(1)))
        case _ => (m, CycleFound(FindPathAdj(g, n, n)))
      }
      case t => t
    }
  }

  def KeepIfIn(l: List[Int], b: List[Boolean]): List[Int] = {
    l match {
      case head :: next => if (b.nthDefault(head, false)) head :: KeepIfIn(next, b) else KeepIfIn(next, b)
      case Nil => Nil
    }
  }

  def TopSortAdj(g: List[List[Int]]): TopSortResult = {
    def helper(g: List[List[Int]]): TopSortResult = {
      val l = g.map(_.length).foldLeft(g.length)( _ + _ )
      ((l - 1) to 0 by -1).foldLeft[(List[Marking.Value], TopSortResult)]((Nil, TotalOrdering(Nil)))((t, i) => TopSortVisit(l, g, t, i)) match {
        case (_, r) => r
      }
    }

    val c = EdgesWithConnections(g)
    helper(g) match {
      case TotalOrdering(l) => TotalOrdering(KeepIfIn(l, c))
      case t => t
    }
  }

  def TopSort(g: List[(Int, Int)]): TopSortResult = TopSortAdj(AdjacencyListFromEdges(g))
}