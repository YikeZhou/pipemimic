package pipemimic

import scala.annotation.tailrec

trait AcyclicCheck {

  /* Adjacency Lists */
  type AdjacencyList = List[List[Int]]

  def AdjacencyListFromEdges(l: List[(Int, Int)]): AdjacencyList = {
    l match {
      case head :: next => AdjacencyListFromEdges(next).appendToNth(head._1, head._2)
      case Nil => Nil
    }
  }

  /* Dijkstra's algorithm */

  private def DijkstraStep(g: AdjacencyList, src: Int, queue: List[Int], prevs: List[Option[Int]], v: Int): (List[Int],
    List[Option[Int]]) = {
    require(v >= 0)
    val adjacentNodes = if (v < g.length) g(v) else List.empty[Int]
    if (adjacentNodes.contains(src)) {
      /* cyclic - stop here */
      (List.empty[Int], prevs.replaceNthIfNone(src, v))
    } else {
      val f_fold = (l: List[Option[Int]], i: Int) => l.replaceNthIfNone(i, v)
      (
        adjacentNodes.foldLeft(queue)((l, i) => l.addUnique(i)),
        adjacentNodes.foldLeft(prevs)(f_fold)
      )
    }
  }

  def DijkstraAdj(g: AdjacencyList, src: Int): (List[Int], List[Option[Int]]) = {
    require(src >= 0)
    @tailrec
    def helper(unroll: Int, g: AdjacencyList, src: Int, queue: List[Int], reachable: List[Int], prevs: List[Option[Int]]): (List[Int], List[Option[Int]]) = {
      require(unroll >= 0)
      if (unroll == 0) {
        (reachable, prevs) /* shouldn't happen ! */
      } else {
        queue match {
          case Nil => (reachable, prevs)
          case head :: _ =>
            val temp = DijkstraStep(g, src, queue, prevs, head)
            helper(unroll - 1, g, src, temp._1.tailError, reachable.addUnique(head), temp._2)
        }
      }
    }

    val max_iters = g.map(_.length).sum + g.length
    val r_prevs = helper(max_iters, g, src, List(src), List.empty, List.empty)
    val r = r_prevs._1
    val prevs = r_prevs._2
    val nth_prevs = if (src < prevs.length) prevs(src) else None
    nth_prevs match {
      case Some(_) => (r, prevs)
      case None => (r.tailError, prevs)
    }
  }

  def Dijkstra(g: List[(Int, Int)], src: Int): (List[Int], List[Option[Int]]) = {
    DijkstraAdj(AdjacencyListFromEdges(g), src)
  }

  /* Path Backtrace */

  def PathBacktrace(g: List[(Int, Int)], prev: List[Option[Int]], src: Int, dst: Int): List[Int] = {
    require(src >= 0 && dst >= 0)

    def helper(prev: List[Option[Int]], src: Int, dst: Int, unroll: Int): List[Int] = {
      require(src >= 0 && dst >= 0 && unroll >= 0)

      val prevs = if (dst < prev.length) prev(dst) else None
      assert(unroll != 0, "PathBacktrace' iteration limit exceeded")
      prevs match {
        case Some(value) =>
          if (value == src) {
            List(value)
          } else {
            helper(prev, src, value, unroll - 1).appended(value)
          }
        case None => /* shouldn't reach here */ List.empty
      }
    }

    val r = helper(prev, src, dst, g.length)
    r match {
      case Nil => Nil
      case _ => r.appended(dst)
    }
  }

  def PathBacktraceAdj(g: AdjacencyList, prev: List[Option[Int]], src: Int, dst: Int): List[Int] = {
    require(src >= 0 && dst >= 0)

    def helper(prev: List[Option[Int]], src: Int, dst: Int, unroll: Int): List[Int] = {
      require(src >= 0 && dst >= 0 && unroll >= 0)

      val prevs = if (dst < prev.length) prev(dst) else None
      assert(unroll != 0 && prevs.isDefined, "PathBacktrace' iteration limit exceeded")
      prevs match {
        case Some(value) =>
          if (value == src) {
            List(value)
          } else {
            helper(prev, src, value, unroll - 1).appended(value)
          }
        case None => /* shouldn't reach here */ List.empty
      }
    }

    val r = helper(prev, src, dst, g.length)
    r match {
      case Nil => Nil
      case _ => r.appended(dst)
    }
  }

  def FindPath(g: List[(Int, Int)], src: Int, dst: Int): List[Int] = {
    Dijkstra(g, src) match {
      case (_, prev) => PathBacktrace(g, prev, src, dst)
    }
  }

  def FindPathAdj(g: AdjacencyList, src: Int, dst: Int): List[Int] = {
    DijkstraAdj(g, src) match {
      case (_, prev) => PathBacktraceAdj(g, prev, src, dst)
    }
  }


  /* Topological Sort Result */

  sealed abstract class TopSortResult

  case class TotalOrdering(l: List[Int]) extends TopSortResult
  case class CycleFound(l: List[Int]) extends TopSortResult

  object Marking extends Enumeration {
    val Unmarked, MarkedTemp, Marked = Value
  }

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

  def VerifyMustHappenBeforeInGraph(g: List[(Int, Int, String)], sd: (Int, Int)): MHBResult = {
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
}
