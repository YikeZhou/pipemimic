package pipemimic.execution

import pipemimic.topology.PathFinder
import pipemimic._

import scala.collection.mutable.ListBuffer

trait ReadsFromEdge extends GlobalGraphID {

  private def rfPerformPairs(src: PathOption, dst: PathOption) = {
    val srcCore = src.evt.iiid.proc
    val dstCore = dst.evt.iiid.proc
    val srcPerfStages = performStagesWithRespectToCore(dstCore, src)
    val dstPerfStages = visibleStagesWithRespectToCore(srcCore, dst)
    CartesianProduct(srcPerfStages, dstPerfStages)
  }

  private def frFromWritePerformPairs(src: PathOption, dst: PathOption) = {
    val srcCore = src.evt.iiid.proc
    val dstCore = dst.evt.iiid.proc
    val srcPerfStages = performOrInvalidStagesWithRespectToCore(dstCore, src)
    val dstPerfStages = performStagesWithRespectToCore(srcCore, dst)
    CartesianProduct(srcPerfStages, dstPerfStages)
  }

  def rfEdges(wsEdges: GraphTree[GlobalEvent], rfEiidPairs: List[(Eiid, Eiid)], s: Scenario, p: Pipeline)
  : GraphTree[GlobalEvent] = {

    /* find following write events in edges for src */
    def reachableVerticesAtLocation(src: GlobalEvent, edges: List[(GlobalEvent, GlobalEvent, String)]): List[Eiid] = {
      val (_, eiid) = src

      s.pathOfEvent(eiid) match {
        case Some(pathOption) =>
          val predecessor = pathOption.evt

          val reachableVertices: List[GlobalEvent] = {
            val rawGraph = edges.map(gepid(p, _)) map { case (s, d, _) => (s, d) }
            PathFinder(rawGraph, geid(p, src)).getAllReachableNodes.map(ungeid(p, _))
          }

          reachableVertices
            .map { case (_, eiid) => eiid }
            /* successor write event has same address */
            .filter { eiid => s.pathOfEvent(eiid).isDefined && s.pathOfEvent(eiid).get.evt.addr == predecessor.addr }
            /* successor is write event */
            .filter { eiid => s.pathOfEvent(eiid).isDefined && s.pathOfEvent(eiid).get.evt.isWrite }
            .toSet /* remove redundant eiid */
            .filterNot(_ == eiid)
            .toList

        case None => Nil
      }
    }

    /* calculate rf edges */
    val readsFromEdges = ListBuffer.empty[GraphTree[GlobalEvent]]

    for (readsFrom <- rfEiidPairs) {
      /* add one reads from edge into list buffer */
      readsFromEdges += {
        val (write, read) = readsFrom /* eiid of write and read events */
        (s.pathOfEvent(write), s.pathOfEvent(read)) match {
          case (Some(pathOfWrite), Some(pathOfRead)) =>
            /* given path of read event and write event, return rf edges and fr edges if exists */

            /* one rf pair may correspond multiple edges */
            val rfPossibilities = {
              val performLocationPairs = rfPerformPairs(pathOfWrite, pathOfRead)

              performLocationPairs map { case (writePerformLocation, readPerformLocation) =>
                val writeGlobalEvent = (writePerformLocation, write)
                val readGlobalEvent = (readPerformLocation, read)
                val e = (writeGlobalEvent, readGlobalEvent, "RF") /* one rf edge */
                val le = List(e)

                /* output a edge */
                def PrintPossibility: GraphTree[GlobalEvent] => String =
                  t => t.toString(GlobalEventString(p, _)) + '\n'
                println(PrintPossibility(GraphTreeLeaf("rf_uhb", le)))

                /* check if exists fr edge */
                val fr = {
                  /* given a RF edge (w, r), for all vertices w' in uhb graph such that (w, w') is an edge
                  between events at the same location, add the fr edge (r, w') */
                  val allWSEdges = wsEdges.flatten
                  val frEdges = allWSEdges map { case (wsName, wsCandidate) =>
                    val reachableWrite = reachableVerticesAtLocation(writeGlobalEvent, wsCandidate)
                    val pathOfReachableWrite = reachableWrite.map(s.pathOfEvent(_))
                      .filter(_.isDefined).map{ case Some(p) => p }
                    (
                      wsName,
                      pathOfReachableWrite.flatMap(w => frFromWritePerformPairs(pathOfRead, w).map { case (rLoc, wLoc) =>
                        ((rLoc, read), (wLoc, w.evt.eiid), "FRfw") /* fr edge */
                      })
                    )
                  }
                  GraphTree(frEdges) // FIXME check this bad usage of GraphTree apply method
                }
                GraphTree(TreeNodeType.And, List(fr, GraphTreeLeaf(executionEdgeLabel("rf", le), le)))
              }
            }
            println(s"Source path ${pathOfWrite.optionName}, Dest path ${pathOfRead.optionName}\n")
            println(s"Architectural RF edge: ${rfPossibilities.length} uhb candidates\n")
            GraphTree(TreeNodeType.Or, rfPossibilities)

          case _ => sys.error("ScenarioExecutionEdges_RF_fromwrite: event not in scenario")
        }
      }
    }

    GraphTree(TreeNodeType.And, readsFromEdges.toList)
  }
}
