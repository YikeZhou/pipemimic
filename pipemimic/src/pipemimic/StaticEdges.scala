package pipemimic

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait StaticEdges {
  def get(p: Pipeline, s: Scenario): GlobalGraph
}

class BasicStaticEdges extends StaticEdges {
  override def get(p: Pipeline, s: Scenario): GlobalGraph = Nil
}

trait IntraLocationEdges extends StaticEdges {


  /** generate intra-location edges */
  abstract override def get(p: Pipeline, s: Scenario): GlobalGraph = {
    val edges = new ArrayBuffer[(GlobalEvent, GlobalEvent, String)]()
    /** get list of paths for each event */
    val pathForEachEvent: PathMap = s.map(/* get path for each event */_.path).map(/* calc edges */_.pairConsecutive)

    /** all local reordering for each location in pipeline p */
    val localReorderingForEachLocation = p.stages.map(_.localReordering)

    val eventsSortedByFirstLocation = mutable.Map.empty[Int, ArrayBuffer[Event]]
    s foreach {
      case PathOption(_, evt, path, _, _) =>
        val firstLocation = path.head
        eventsSortedByFirstLocation.getOrElseUpdate(firstLocation, ArrayBuffer.empty[Event]) += evt
    }

    val allEdges = Array.fill(p.stages.length)(new ArrayBuffer[(Event, Event)]())

    def programOrderEdges(events: mutable.Seq[Event]): mutable.Seq[(Event, Event)] = {
      /* sorted by processor index */
      val processorIndices = events.map(_.iiid.proc).toSet
      assert(processorIndices.size == 1) /* input events always on same core no need to sort them by core id */
      val sortedEvents = events.sortWith(_.iiid.poi < _.iiid.poi)

      def transitiveClosure[A](l: mutable.Seq[A]): mutable.Seq[(A, A)] = {
        val tc = ArrayBuffer.empty[(A, A)]
        for (i <- l.indices) {
          tc.addAll(l.drop(i + 1).map((l(i), _)))
        }
        tc
      }
      /* generate all program order edges (not sorted by processor ids in return value) */
      transitiveClosure(sortedEvents) // FIXME when events more than 2 there will be too much edges
    }

    def transferredEdgesAtLocation(loc: Location, reordering: LocalReordering): List[(Event, Event)] = {

      def isTransferredEdge(s: Location, d: Location, e: (Event, Event)): Boolean = {
        val path1 = pathForEachEvent(e._1.eiid)
        val path2 = pathForEachEvent(e._2.eiid)
        path1.contains((s, d)) && path2.contains((s, d))
      }

      def transferredEdges(candidates: mutable.Seq[(Event, Event)], s: Location, d: Location)
      : mutable.Seq[(Event, Event)] = candidates.filter(isTransferredEdge(s, d, _))

      val edgesAtLoc = mutable.Set.empty[(Event, Event)]

      (0 until loc).foreach { start =>
        /* check pair: (start, loc) for transferred edge */
        val lastStageEdges = transferredEdges(allEdges(start), start, loc)
        edgesAtLoc.addAll(reordering(allEdges.take(loc).map(_.toList).toList)(lastStageEdges.toList))
      } /* NOTE for local reordering 'restore', this algorithm may yield repeated edges if not using set */

      edgesAtLoc.toList
    }

    localReorderingForEachLocation.zipWithIndex foreach {
      case (reordering, location) if eventsSortedByFirstLocation.contains(location) =>
        /* When there are events starting at current location, calculate their program order edges and add them into
        allEdges. This can be done by calculating transitive closure among these events.
         */
        allEdges(location).addAll(transferredEdgesAtLocation(location, reordering))
        allEdges(location).addAll(programOrderEdges(eventsSortedByFirstLocation(location)))
      case (reordering, location) => /* eventsSortedByFirstLocation doesn't contain location */
        /* When there are no more events left (meaning that no more events start at current location), keep calculate
        intra location edges (enforced by local reordering) until the last location in the pipeline.
         */
        allEdges(location).addAll(transferredEdgesAtLocation(location, reordering))
    }

    /** Turn [[allEdges]] into global edges */
    allEdges.zipWithIndex foreach {
      case (intraLocationEdges, location) => intraLocationEdges foreach {
        case (former, latter) => /* former happens before latter */
          edges.addOne((location, former.eiid), (location, latter.eiid), "IntraLocation")
      }
    }

    super.get(p, s) ::: edges.toList
  }
}

trait IntraEventEdges extends StaticEdges  {


  abstract override def get(p: Pipeline, s: Scenario): GlobalGraph = {
    val edges = new ArrayBuffer[(GlobalEvent, GlobalEvent, String)]()
    s foreach {
      case PathOption(_, evt, path, _, _) =>
        path.pairConsecutive("IntraEvent") map {
          case (s, d, str) => edges.addOne((s, evt.eiid), (d, evt.eiid), str)
        }
    }

    super.get(p, s) ::: edges.toList
  }
}

trait PipelineSpecialEdges extends StaticEdges {


  abstract override def get(p: Pipeline, s: Scenario): GlobalGraph = {
    val edges = new ArrayBuffer[(GlobalEvent, GlobalEvent, String)]()
    val allEvents = s.map(_.evt)

    for (pathOption <- s) {
      val e = pathOption.evt
      for (loc <- pathOption.path) if (p.stages(loc).specialEdges != pipeline.NoSpecialEdges) {
        /* special edges need to add */
        val sem = p.stages(loc).specialEdges
        val localEvents = allEvents.filter(_.iiid.proc == e.iiid.proc)
        val (before, after) = localEvents.filterNot(_.eiid == e.eiid).partition(_.iiid.poi < e.iiid.poi)
        edges.addAll(sem(before)(e)(after))
      }
    }

    super.get(p, s) ::: edges.toList
  }
}

trait PathSpecialEdges extends StaticEdges {

  abstract override def get(p: Pipeline, s: Scenario): GlobalGraph = {
    val edges = new ArrayBuffer[(GlobalEvent, GlobalEvent, String)]()

    val eventsSortedByProc = Array.fill(p.coreNumber)(ArrayBuffer.empty[Event])
    s.map(_.evt) foreach {e =>
      eventsSortedByProc(e.iiid.proc) += e
    }
    s foreach {
      case PathOption(_, evt, _, _, sem) if sem != pipeline.NoSpecialEdges =>
        val localEvents = eventsSortedByProc(evt.iiid.proc)
        val (before, after) = localEvents.filterNot(_.eiid == evt.eiid).partition(_.iiid.poi < evt.iiid.poi)
        edges.addAll(sem(before.toList)(evt)(after.toList))
      case _ =>
    }

    super.get(p, s) ::: edges.toList
  }
}

class ScenarioStaticEdges
  extends BasicStaticEdges
    with IntraLocationEdges
    with IntraEventEdges
    with PipelineSpecialEdges
    with PathSpecialEdges

object StaticEdges {
  def apply(title: String, pipeline: Pipeline, scenario: Scenario): GraphTree[GlobalEvent] = {
    val se = new ScenarioStaticEdges
    GraphTreeLeaf(title, se.get(pipeline, scenario))
  }
}