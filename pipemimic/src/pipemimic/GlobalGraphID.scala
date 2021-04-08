package pipemimic

import scala.annotation.tailrec

import Stages._

trait GlobalGraphID {
  def geid(p: Pipeline, ge: GlobalEvent): Int = {
    ge match {
      case (n, e) => e * p.stages.length + n
    }
  }

  def gepid(p: Pipeline, gep: (GlobalEvent, GlobalEvent, String)): (Int, Int, String) = {
    (geid(p, gep._1), geid(p, gep._2), gep._3)
  }

  def getid(p: Pipeline, t: GraphTree[GlobalEvent]): GraphTree[Int] = { // TODO: Pipeline => pipeline.stages.length
    t match {
      case GraphTreeOr(l) => GraphTreeOr(l.map(getid(p, _)))
      case GraphTreeAnd(l) => GraphTreeAnd(l.map(getid(p, _)))
      case GraphTreeLeaf(s, l) => GraphTreeLeaf(s, l.map(gepid(p, _)))
    }
  }

  def ungeid(p: Pipeline, n: Int): (Stages.Location, ProgramOrderIndex) = {
    @tailrec
    def helper(p: Pipeline, n: Int, s: Int, e: Int): (Stages.Location, ProgramOrderIndex) = {
      if (s == p.stages.length) {
        if (n == 0) (0, e + 1) else helper(p, n - 1, 1, e + 1)
      } else if (s < p.stages.length) {
        if (n == 0) (s, e) else helper(p, n - 1, s + 1, e)
      } else {
        /* ERROR */ (0, 0)
      }
    }

    helper(p, n, 0, 0)
  }

}