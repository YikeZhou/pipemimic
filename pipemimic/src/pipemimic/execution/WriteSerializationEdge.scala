package pipemimic.execution

import pipemimic.Stages.{GlobalEvent, PathOption, PerformStages, Scenario}
import pipemimic.organization.Interleaving
import pipemimic.{Direction, GraphTree, GraphTreeAnd, GraphTreeLeaf, GraphTreeOr, ListImprovements}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * To calculate all of the WS edges, we do the following:
  * 1) Sort the events in the scenario by location
  * 2) Sort each per-location list of events by issuing core, so that we have a list of lists of events in per-location
  * per-core program order
  * 3) Calculate the set of all interleavings of stores to each location among the different cores
  * 4) Generate the list of WS edges for each interleaving
  * 5) Combine the results for each individual location into a GraphTree
  */
trait WriteSerializationEdge {
  private val coreCnt = 2 /* only called when running litmus tests and core number must be 2 */
  private val memoryCell = 3 /* defined in litmus test */

  private def sortByCore(s: Scenario): Array[Array[GlobalEvent]] = {
    val sorted = Array.fill(coreCnt)(ArrayBuffer.empty[GlobalEvent])

    for (path <- s) {
      /* write serialization must happen in shared cache or main memory */
      val performStage = path.performStages.find(_.isMainMemory)

      performStage match {
        case Some(PerformStages(stage, cores, observability, cacheLineInvLoc, isMainMemory)) =>
          /* append to proc-th array in sorted */
          sorted(path.evt.iiid.proc) += ((stage, path.evt.eiid))
        case _ =>
      }
    }
    sorted.map(_.toArray)
  }

  private def sortByAddress(s: Scenario): Array[Scenario] = {
    val sorted = Array.fill(memoryCell)(ListBuffer.empty[PathOption])

    for (path <- s) {
      (path.evt.dirn, path.evt.addr) match {
        case (Some(Direction.W), Some(addr)) => sorted(addr) += path
        case _ =>
      }
    }
    sorted.map(_.toList)
  }


  def wsEdges(s: Scenario): GraphTree[GlobalEvent] = {
    val edgesPerInterleaving = {
      val sortedByLocation = sortByAddress(s)
      val writeEventsSortByLocThenCore = sortedByLocation.map(sortByCore)
      /* element in wsCandidateForEachLocation : all possibilities of write serialization for location 0~n */
      val wsCandidateForEachLocation = writeEventsSortByLocThenCore.map(Interleaving[GlobalEvent](_: _*))
      /* for each possible case, generate a edge list */
      /* when there is only 1 write op at location, no ws edge will be generated */
      wsCandidateForEachLocation.map(_.map(_.pairConsecutive("WS")))
    }
    /* turn list of global edges into a graph tree */
    val rawGraph = GraphTreeAnd(edgesPerInterleaving.toList.map { wsCandidatesAtLocation =>
      val wsPossibilities = wsCandidatesAtLocation map { candidate =>
        /* one possible write serialization */
        GraphTreeLeaf(executionEdgeLabel("ws", candidate), candidate)
      }
      println(s"WS @ location: ${wsPossibilities.length} candidates\n")
      GraphTreeOr(wsPossibilities)
    })
//    println(rawGraph)
    rawGraph
  }
}