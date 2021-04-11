package pipemimic

import scala.annotation.tailrec

trait GlobalGraphID {
  /**
    * Given a global event, return its stage index in pipeline
    * @param p pipeline
    * @param ge global event
    * @return index in all stages of pipeline p
    */
  def globalEventID(p: Pipeline, ge: GlobalEvent): Int = {
    ge match {
      case (location, eiid) => eiid * p.stages.length + location
    }
  }

  /**
    * Given a pair of global event (i.e. edge in global graph), return corresponding edge in raw graph
    * @param p pipeline
    * @param gep global event pair
    * @return edge in raw graph
    */
  def globalEventEdgeID(p: Pipeline, gep: (GlobalEvent, GlobalEvent, String)): (Int, Int, String) = {
    (globalEventID(p, gep._1), globalEventID(p, gep._2), gep._3)
  }

  /**
    * convert graph tree of global event into tree of integer value
    * @param p pipeline
    * @param t graph tree of global event
    * @return integer tree
    */
  def globalGraphID(p: Pipeline, t: GraphTree[GlobalEvent]): GraphTree[Int] = {
    t match {
      case GraphTreeOr(l) => GraphTreeOr(l.map(globalGraphID(p, _)))
      case GraphTreeAnd(l) => GraphTreeAnd(l.map(globalGraphID(p, _)))
      case GraphTreeLeaf(s, l) => GraphTreeLeaf(s, l.map(globalEventEdgeID(p, _)))
    }
  }

  /**
    * Reverse of [[globalEventID]]. Used to convert raw graph into formatted dot graph.
    * @param p pipeline
    * @param n index of stage in pipeline p
    * @return corresponding global event
    */
  def ungeid(p: Pipeline, n: Int): (Location, Eiid) = {
    @tailrec
    def helper(p: Pipeline, n: Int, s: Int, e: Int): (Location, Eiid) = {
      if (s == p.stages.length) {
        if (n == 0) (0, e + 1) else helper(p, n - 1, 1, e + 1)
      } else if (s < p.stages.length) {
        if (n == 0) (s, e) else helper(p, n - 1, s + 1, e)
      } else {
        /* ERROR */ (0, 0)
      }
    }
    // FIXME check algorithm here
    helper(p, n, 0, 0)
  }

}
