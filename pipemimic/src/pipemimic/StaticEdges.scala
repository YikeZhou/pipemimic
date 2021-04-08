//package pipemimic
//
//import scala.collection.mutable
//import scala.collection.mutable._
//import Stages._
//
//trait HasEdgeBuffer {
//  val edges = new ArrayBuffer[(GlobalEvent, GlobalEvent, String)]()
//}
//
//abstract class StaticEdges {
//  def get(p: Pipeline, s: Scenario): GlobalGraph
//}
//
//class BasicStaticEdges extends StaticEdges {
//  override def get(p: Pipeline, s: Scenario): GlobalGraph = Nil
//}
//
//trait IntraLocationEdges extends StaticEdges {
//
//
//  /** generate intra-location edges */
//  abstract override def get(p: Pipeline, s: Scenario): GlobalGraph = {
//    val edges = new ArrayBuffer[(GlobalEvent, GlobalEvent, String)]()
//    /** get list of paths for each event */
//    val pathForEachEvent: PathMap = s.map(/* get path for each event */_.path).map(/* calc edges */_.pairConsecutive)
//
//    /** all local reordering for each location in pipeline p */
//    val localReorderingForEachLocation = p.stages.map(_.localReordering)
//
//    val eventsSortedByFirstLocation = Array.fill(p.stages.length)(new ArrayBuffer[Event]())
//    s foreach {
//      case PathOption(optionName, evt, path, performStages, sem) =>
//        val firstLocation = path.head
//        eventsSortedByFirstLocation(firstLocation) += evt
//    }
//
//    val allEdges = Array.fill(p.stages.length)(new ArrayBuffer[(Event, Event)]())
//
//    def programOrderEdges(events: mutable.Seq[Event]): mutable.Seq[(Event, Event)] = {
//      /* sorted by processor index */
//      val processorIndices = events.map(_.iiid.proc)
//      val maxProcIndex = processorIndices.max // FIXME add a procCnt field to class Pipeline
//      val sortedEvents = Array.fill(maxProcIndex + 1)(new ArrayBuffer[Event]())
//      events foreach (e => sortedEvents(e.iiid.proc) += e)
//
//      def transitiveClosure[A](l: mutable.Seq[A]): mutable.Seq[(A, A)] = {
//        val tc = ArrayBuffer.empty[(A, A)]
//        for (i <- l.indices) {
//          tc.addAll(l.drop(i + 1).map((l(i), _)))
//        }
//        tc
//      }
//      /* generate all program order edges (not sorted by processor ids in return value) */
//      sortedEvents.flatMap(transitiveClosure)
//    }
//
//    def transferredEdgesAtLocation(loc: Location, reordering: LocalReordering): List[(Event, Event)] = {
//
//      def isTransferredEdge(s: Location, d: Location, e: (Event, Event)): Boolean = {
//        val path1 = pathForEachEvent(e._1.eiid)
//        val path2 = pathForEachEvent(e._2.eiid)
//        path1.contains((s, d)) && path2.contains((s, d))
//      }
//
//      def transferredEdges(candidates: mutable.Seq[(Event, Event)], s: Location, d: Location)
//      : mutable.Seq[(Event, Event)] = candidates.filter(isTransferredEdge(s, d, _))
//
//      (0 until loc).flatMap { start =>
//        /* check pair: (start, loc) for transferred edge */
//        val lastStageEdges = transferredEdges(allEdges(start), start, loc)
//        localReorderingForEachLocation(loc)(allEdges.take(loc).map(_.toList).toList)(lastStageEdges.toList)
//      }.toList
//    }
//
//    localReorderingForEachLocation.zipWithIndex zip eventsSortedByFirstLocation foreach {
//      case ((reordering, location), events) if events.nonEmpty =>
//        /* When there are events starting at current location, calculate their program order edges and add them into
//        allEdges. This can be done by calculating transitive closure among these events.
//         */
//        allEdges(location).addAll(transferredEdgesAtLocation(location, reordering))
//        allEdges(location).addAll(programOrderEdges(events))
//      case ((reordering, location), events) if events.isEmpty =>
//        /* When there are no more events left (meaning that no more events start at current location), keep calculate
//        intra location edges (enforced by local reordering) until the last location in the pipeline.
//         */
//        allEdges(location).addAll(transferredEdgesAtLocation(location, reordering))
//    }
//
//    /** Turn [[allEdges]] into global edges */
//    allEdges.zipWithIndex foreach {
//      case (intraLocationEdges, location) => intraLocationEdges foreach {
//        case (former, latter) => /* former happens before latter */
//          edges.addOne((location, former.eiid), (location, latter.eiid), "IntraLocation")
//      }
//    }
//
//    super.get(p, s) ::: edges.toList
//  }
//}
//
//trait IntraEventEdges extends StaticEdges  {
//
//
//  abstract override def get(p: Pipeline, s: Scenario): GlobalGraph = {
//    val edges = new ArrayBuffer[(GlobalEvent, GlobalEvent, String)]()
//    s foreach {
//      case PathOption(optionName, evt, path, performStages, sem) =>
//        path.pairConsecutive("IntraEvent") map {
//          case (s, d, str) => edges.addOne((s, evt.eiid), (d, evt.eiid), str)
//        }
//    }
//
//    super.get(p, s) ::: edges.toList
//  }
//}
//
//trait PipelineSpecialEdges extends StaticEdges {
//
//
//  abstract override def get(p: Pipeline, s: Scenario): GlobalGraph = {
//    val edges = new ArrayBuffer[(GlobalEvent, GlobalEvent, String)]()
//    val allEvents = s.map(_.evt)
//
//    for (pathOption <- s) {
//      val e = pathOption.evt
//      for (loc <- pathOption.path) if (p.stages(loc).specialEdges != Pipeline.NoSpecialEdges) {
//        /* special edges need to add */
//        val sem = p.stages(loc).specialEdges
//        val localEvents = allEvents.filter(_.iiid.proc == e.iiid.proc)
//        val (before, after) = localEvents.filterNot(_.eiid == e.eiid).partition(_.iiid.poi < e.iiid.poi)
//        edges.addAll(sem(before)(e)(after))
//      }
//    }
//
//    super.get(p, s) ::: edges.toList
//  }
//}
//
//trait PathSpecialEdges extends StaticEdges {
//
//  abstract override def get(p: Pipeline, s: Scenario): GlobalGraph = {
//    val edges = new ArrayBuffer[(GlobalEvent, GlobalEvent, String)]()
//
//    val eventsSortedByProc = Array.fill(p.cores)(ArrayBuffer.empty[Event])
//    s.map(_.evt) foreach {e =>
//      eventsSortedByProc(e.iiid.proc) += e
//    }
//    s foreach {
//      case PathOption(optionName, evt, path, performStages, sem) =>
//        val localEvents = eventsSortedByProc(evt.iiid.proc)
//        val (before, after) = localEvents.filterNot(_.eiid == evt.eiid).partition(_.iiid.poi < evt.iiid.poi)
//        edges.addAll(sem(before.toList)(evt)(after.toList))
//    }
//
//    super.get(p, s) ::: edges.toList
//  }
//}
//
//class ScenarioStaticEdges
//  extends BasicStaticEdges
//    with IntraLocationEdges
//    with IntraEventEdges
//    with PipelineSpecialEdges
//    with PathSpecialEdges
//
//object StaticEdges {
//  def scenarioStaticEdges(title: String, pipeline: Pipeline, scenario: Scenario): GraphTree[GlobalEvent] = {
//    val se = new ScenarioStaticEdges
//    GraphTreeLeaf(title, se.get(pipeline, scenario))
//  }
//}