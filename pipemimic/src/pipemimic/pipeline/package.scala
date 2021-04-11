package pipemimic

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

package object pipeline {
  // TODO add useful definitions such as FIFO, NoSpecialEdges, etc.

  /* Local Reordering */

  /**
    * any ordering guaranteed at the input is also guaranteed at the output. This is the common case.
    */
  val FIFO: LocalReordering = _ => ordering => ordering

  /**
    * Operations can leave the stage in any order; nothing is guaranteed.
    */
  val NoOrderGuarantees: LocalReordering = _ => _ => Nil

  /**
    * The output order is guaranteed to match some previous ordering
    */
  def Restore(n: Int): LocalReordering = e => _ => e.nthDefault(n, Nil)

  /* Special Edge Maps */

  /**
    * In most cases, we don't need to add any special edges
    */
  val NoSpecialEdges: SpecialEdgeMap = _ => _ => _ => Nil

  @tailrec
  def StoreBufferSpecialEdges(c: Int, n: Int)(eventBefore: List[Event])(e: Event)(eventAfter: List[Event]): GlobalGraph = {
    eventAfter match {
      case h :: t => h.dirn match {
        case Some(Direction.R) => StoreBufferSpecialEdges(c, n)(eventBefore)(e)(t)
        case Some(Direction.W) => List(((6 * n, e.eiid), (5 + 6 * c, h.eiid), "StoreBuffer"))
        case _ => StoreBufferSpecialEdges(c, n)(eventBefore)(e)(t)
      }
      case Nil => Nil
    }
  }

  def FenceTSOSpecialEdges(c: Int, n: Int)(eventBefore: List[Event])(e: Event)(eventAfter: List[Event]): GlobalGraph = {
    require(e.action.isInstanceOf[MemoryFence])
    val edges = ListBuffer.empty[((Location, Eiid), (Location, Eiid), String)]
    eventBefore foreach {
      case Event(eiid, Iiid(proc, _), action) => action match {

        case Access(Direction.R, _, _) =>
          /* happens before all events in eventsAfter */
          for (latter <- eventAfter) {
            if (latter.dirn.contains(Direction.W))
              edges += (((6 * proc + 3, eiid), (6 * n, latter.eiid), "FenceTSO"))
            else if (latter.dirn.contains(Direction.R))
              edges += (((6 * proc + 3, eiid), (6 * proc + 3, latter.eiid), "FenceTSO"))
          }

        case Access(Direction.W, _, _) =>
          /* happens before all store events in eventsAfter */
          for (latter <- eventAfter if latter.dirn.contains(Direction.W))
            edges += (((6 * n, eiid), (6 * n, latter.eiid), "FenceTSO"))

        case _ =>
      }
    }
    edges.toList
  }
}
