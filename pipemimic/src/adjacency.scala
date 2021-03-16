package pipemimic

import ListUtils._

object Adjacency {

  /* Adjacency Lists */
  type AdjacencyList = List[List[Int]]

  def AdjacencyListFromEdges(l: List[Tuple2[Int, Int]]): AdjacencyList = {
    l match {
      case head :: next => AppendToNth(AdjacencyListFromEdges(next), head._1, head._2)
      case Nil => Nil
    }
  }

  /* Dijkstra's algorithm */
  
  def DijkstraStep(g: AdjacencyList, src: Int, queue: List[Int], prevs: List[Option[Int]], v: Int): Tuple2[List[Int], List[Option[Int]]] = {
    require(v >= 0)
    val adjacentNodes = if (v < g.length) g(v) else List.empty[Int]
    if (adjacentNodes.contains(src)) {
      /* cyclic - stop here */
      (List.empty[Int], replaceNthIfNone(prevs, src, v))
    } else {
      val f_fold = (l: List[Option[Int]], i: Int) => replaceNthIfNone(l, i, v)
      (
        adjacentNodes.foldLeft(queue)((l, i) => AddUnique(l, i)),
        adjacentNodes.foldLeft(prevs)(f_fold)
      )
    }
  }

  def DijkstraAdj(g: AdjacencyList, src: Int): Tuple2[List[Int], List[Option[Int]]] = {
    require(src >= 0)
    def helper(unroll: Int, g: AdjacencyList, src: Int, queue: List[Int], reachable: List[Int], prevs: List[Option[Int]]): Tuple2[List[Int], List[Option[Int]]] = {
      require(unroll >= 0)
      if (unroll == 0) {
        (reachable, prevs) /* shouldn't happen ! */
      } else {
        queue match {
          case Nil => (reachable, prevs)
          case head :: next => {
            val temp = DijkstraStep(g, src, queue, prevs, head)
            helper(unroll - 1, g, src, Tail(temp._1), AddUnique(reachable, head), temp._2)
          }
        }
      }
    }

    val max_iters = g.map(_.length).foldLeft(g.length)(_ + _)
    val r_prevs = helper(max_iters, g, src, List(src), List.empty, List.empty)
    val r = r_prevs._1
    val prevs = r_prevs._2
    val nth_prevs = if (src < prevs.length) prevs(src) else None
    nth_prevs match {
      case Some(value) => (r, prevs)
      case None => (Tail(r), prevs)
    }
  }

  def Dijkstra(g: List[Tuple2[Int, Int]], src: Int): Tuple2[List[Int], List[Option[Int]]] = {
    DijkstraAdj(AdjacencyListFromEdges(g), src)
  }

  /* Path Backtrace */

  def PathBacktrace(g: List[Tuple2[Int, Int]], prev: List[Option[Int]], src: Int, dst: Int): List[Int] = {
    require(src >= 0 && dst >= 0)

    def helper(prev: List[Option[Int]], src: Int, dst: Int, unroll: Int): List[Int] = {
      require(src >= 0 && dst >= 0 && unroll >= 0)

      val prevs = if (dst < prev.length) prev(dst) else None
      assert(unroll != 0, "PathBacktrace' iteration limit exceeded")
      prevs match {
        case Some(value) => {
          if (value == src) {
            List(value)
          } else {
            helper(prev, src, value, unroll - 1).appended(value)
          }
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
      assert(unroll != 0 && prevs != None, "PathBacktrace' iteration limit exceeded")
      prevs match {
        case Some(value) => {
          if (value == src) {
            List(value)
          } else {
            helper(prev, src, value, unroll - 1).appended(value)
          }
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

  def FindPath(g: List[Tuple2[Int, Int]], src: Int, dst: Int): List[Int] = {
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