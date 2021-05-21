package pipemimic

import scala.collection.mutable.ListBuffer

package object pipeline {

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
    * The output order is guaranteed to match some previous ordering.
    * e.g. reorder buffer
    */
  def restore(globalStageIndex: Int): LocalReordering = e => _ => e(globalStageIndex) filter {
    case (former, latter) => former.addr == latter.addr
  }

  /** Only operations to the same address are guaranteed to maintain their ordering. */
  def sameAddressOrdered: LocalReordering = _ => predecessor => predecessor filter {
    case (former, latter) => former.addr == latter.addr
  }

  /* Special Edge Maps */

  /**
    * In most cases, we don't need to add any special edges
    */
  val NoSpecialEdges: SpecialEdgeMap = _ => _ => _ => Nil

  /** This store buffer only allows one outstanding unacknowledged store at a time. */
  def storeBufferSpecialEdges(srcPerformStage: Location, dstPerformStage: Location)
                             (eventBefore: List[Event])(e: Event)(eventAfter: List[Event]): GlobalGraph = {
    eventAfter foreach { after =>
      if (after.isWrite) {
        /* current store must write to memory before next store value enter store buffer
        * Used when store buffer size = 1 (hold 1 write a time) or define a total store order */
        return List(((srcPerformStage, e.eiid), (dstPerformStage, after.eiid), "StoreBuffer"))
      }
    }
    Nil
  }

  def readAfterWriteSpecialEdges(writePerformStage: Location, readPerformStage: Location)
                                (eventBefore: List[Event])(e: Event)(eventAfter: List[Event]): GlobalGraph = {
    require(e.isWrite)
    /* find first read with same addr */
    eventAfter foreach { after =>
      if (after.isRead && after.addr == e.addr) {
        /* read must stall until write finished */
        return List(((writePerformStage, e.eiid), (readPerformStage, after.eiid), "DataConflict"))
      }
    }
    Nil
  }

  /** Fence instruction will go through five stage in risc pipeline */
  def fenceTSOSpecialEdges(storePerformStage: Location, loadPerformStage: Location)
                          (eventBefore: List[Event])(e: Event)(eventAfter: List[Event]): GlobalGraph = {
    // TODO check implementation in reality
    require(e.action.isInstanceOf[MemoryFence])
    require(eventBefore.forall(!_.action.isInstanceOf[MemoryFence]) &&
      eventAfter.forall(!_.action.isInstanceOf[MemoryFence]))

    val edges = ListBuffer.empty[(GlobalEvent, GlobalEvent, String)]

    /* events before must finish before events after start */
    eventBefore foreach {
      case Event(eiid, Iiid(proc, _), action) => action match {

        case Access(Direction.R, _, _) =>
          /* happens before all events in eventsAfter */
          for (latter <- eventAfter) {
            if (latter.dirn.contains(Direction.W))
              edges += (((loadPerformStage, eiid), (storePerformStage, latter.eiid), "FenceTSO"))
            else if (latter.dirn.contains(Direction.R))
              edges += (((loadPerformStage, eiid), (loadPerformStage, latter.eiid), "FenceTSO"))
          }

        case Access(Direction.W, _, _) =>
          /* happens before all store events in eventsAfter */
          for (latter <- eventAfter if latter.dirn.contains(Direction.W))
            edges += (((storePerformStage, eiid), (storePerformStage, latter.eiid), "FenceTSO"))

        case _ =>
      }
    }

    edges.toList
  }

  /** This is a intra-event edge from read to cache line invalidation */
  def loadCacheLineSpecialEdges(loadPerformStage: Location, cacheInvPerformStage: Location)
                               (eventsBefore: List[Event])(e: Event)(eventsAfter: List[Event]): GlobalGraph = {
    List( ((loadPerformStage, e.eiid), (cacheInvPerformStage, e.eiid), "LoadCacheLine") )
  }

  /** For every load operation, if following load at same address current load should happen before latter cache inv */
  def speculativeLoadReorderingSpecialEdges(loadPerformStage: Location, cacheInvPerformStage: Location)
                                           (eventsBefore: List[Event])(e: Event)(eventsAfter: List[Event]): GlobalGraph = {
    val edges = ListBuffer.empty[(GlobalEvent, GlobalEvent, String)]
    eventsAfter foreach {
      case Event(eiid, _, Access(direction, address, _)) if direction == Direction.R && e.addr.contains(address) =>
        edges += (((loadPerformStage, e.eiid), (cacheInvPerformStage, eiid), "SpeculativeLoadReordering"))
      case _ =>
    }
    edges.toList
  }

  /** special edges for load path option */
  def loadSpecialEdges(loadPerformStage: Location, cacheInvPerformStage: Location)
                      (eventsBefore: List[Event])(e: Event)(eventsAfter: List[Event]): GlobalGraph = {
    loadCacheLineSpecialEdges(loadPerformStage, cacheInvPerformStage)(eventsBefore)(e)(eventsAfter) :::
      speculativeLoadReorderingSpecialEdges(loadPerformStage, cacheInvPerformStage)(eventsBefore)(e)(eventsAfter)
  }

  /** When the store executes, squash following load at same address if it already performed. */
  def storeLoadSpecialEdges(storePerformStage: Location, loadPerformStage: Location)
                           (eventsBefore: List[Event])(e: Event)(eventsAfter: List[Event]): GlobalGraph = {
    require(e.isWrite)

    val edges = ListBuffer.empty[(GlobalEvent, GlobalEvent, String)]
    eventsAfter foreach {
      case Event(eiid, _, Access(direction, address, _)) if direction == Direction.R && e.addr.contains(address) =>
        edges += (((storePerformStage, e.eiid), (loadPerformStage, eiid), "StoreLoad"))
      case _ =>
    }
    edges.toList
  }

}
