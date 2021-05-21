package pipemimic.execution

import pipemimic._

import scala.collection.mutable.ListBuffer

trait FromReadEdge {
  private def writesToSameAddress(addr: Option[Address], s: Scenario): List[PathOption] =
    s.filter(pathOption => !pathOption.evt.isRead && pathOption.evt.addr == addr)

  private def frInitialPerformPairs(src: PathOption, dst: PathOption) = {
    val srcCore = src.evt.iiid.proc
    val dstCore = dst.evt.iiid.proc
    val srcPerfStages = performOrInvalidStagesWithRespectToCore(dstCore, src) /* Read */
    val dstPerfStages = performStagesWithRespectToCore(srcCore, dst) /* Write */
    CartesianProduct(srcPerfStages, dstPerfStages)
  }

  def frEdges(readsFromInitValue: List[Event], s: Scenario, p: Pipeline): GraphTree[GlobalEvent] = {
    val fromReadEdges = ListBuffer.empty[GraphTree[GlobalEvent]]

    for (reads <- readsFromInitValue) {
      /* given event readsFromInitValue, return corresponding fr edge */
      s.pathOfEvent(reads.eiid) match {
        case Some(pathOfReadFromInitial) =>
          /* find write event to same location */
          val writeEventToSameAddress = writesToSameAddress(pathOfReadFromInitial.evt.addr, s)
          writeEventToSameAddress foreach { pathOfWrite => /* readsFromInit -fr-> write */
            frInitialPerformPairs(pathOfReadFromInitial, pathOfWrite) map { case (rLoc, wLoc) =>
              /* add all fr edge into list buffer fromReadEdges */
              val readGlobalEvent = (rLoc, pathOfReadFromInitial.evt.eiid)
              val writeGlobalEvent = (wLoc, pathOfWrite.evt.eiid)
              val e = (readGlobalEvent, writeGlobalEvent, "FRi")
              val le = List(e)
              def PrintPossibility: GraphTree[GlobalEvent] => String =
                t => t.toString(GlobalEventString(p, _)) + '\n'
              println(PrintPossibility(GraphTreeLeaf("fr_uhb", le)))
              fromReadEdges += GraphTreeLeaf(executionEdgeLabel("fr", le), le)
            }
          }
        case None =>
          sys.error("ScenarioExecutionEdges_FR_initial: event is not actually in scenario")
      }
    }

    GraphTree(TreeNodeType.And, fromReadEdges.toList)
  }
}
