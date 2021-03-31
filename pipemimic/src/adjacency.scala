package pipemimic

import scala.annotation.tailrec

object Adjacency {

  /* Adjacency Lists */
  type AdjacencyList = List[List[Int]]

  def AdjacencyListFromEdges(l: List[(Int, Int)]): AdjacencyList = {
    l match {
      case head :: next => AdjacencyListFromEdges(next).appendToNth(head._1, head._2)
      case Nil => Nil
    }
  }

  /* Dijkstra's algorithm */
  
  def DijkstraStep(g: AdjacencyList, src: Int, queue: List[Int], prevs: List[Option[Int]], v: Int): (List[Int], List[Option[Int]]) = {
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
}