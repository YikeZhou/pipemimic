package pipemimic

import ListUtils._

import scala.annotation.tailrec

object Stages {

  /* Local Ordering Graphs */

  type Location = Int /* Location in Local Ordering Graphs */

  /**
    * A [Path] is a list of locations (e.g., pipeline stages), given by [nat] indices, through which an operation passes
    * during its execution.
    */
  type Path = List[Location]

  /* Transitive Closure of a [Path] */

  /**
    * Given a path, generate the list of pairs representing the transitive
    * closure of the path
    *
    * @param l a path
    * @return transitive closure of path l
    */
  def PathTransitiveClosure[A](l: List[A]): List[(A, A)] = {
    /**
      * Pair src with each element of dsts
      *
      * @param src first element of the tuples in return list
      * @param dsts including second elements of the tuples
      * @return list of tuples
      */
    def PathTCPair[B](src: B, dsts: List[B]): List[(B, B)] = {
      dsts match {
        case head :: next => (src, head) :: PathTCPair(src, next)
        case Nil => Nil
      }
    }

    l match {
      case head :: next => PathTCPair(head, next) ::: PathTransitiveClosure(next)
      case Nil => Nil
    }
  }

  /**
    * A path map is a list in which the nth element is the path (as pairs of locations) taken by the nth Event
    * in program order.
    */
  type PathMap = List[List[(Location, Location)]]
  
  /* Graphs of Happens-Before Orderings */

  /**
    * A LocalOrdering is an ordering on Events that is enforced with at a given location (e.g., at a particular pipeline
    * stage.
    */
  type LocalOrdering = List[(Event, Event)]
  /**
    * A LocalReordering takes a set of events and returns some other set of events.
    * This represents the set of guarantees that are maintained, restored, no longer maintained, etc. through a given
    * location.
    * The first argument (List[LocalOrdering]) provides the ordering as seen by all previous locations in the
    * pipeline. We use this to be able to define things like reorder buffers, where the output ordering is defined to be
    * equal to the output ordering of some previous stage, e.g., the decode stage.
    */
  type LocalReordering = List[LocalOrdering] => LocalOrdering => LocalOrdering

  /**
    * If the LocalOrdering edge (e1, e2) is present at location (vertex) v1, and if both e1 and e2 have v2 as the next
    * location after v1 in the Paths they take, then (e1, e2) is a TransferredEdge from v1 to v2.
    * v1: e1 -----> e2
    * v2: e3 --te-> e4
    * @param paths path map
    * @param v1 location 1
    * @param v2 location 2
    * @param e1 event 1
    * @param e2 event 2
    * @return if (e1, e2) is a TransferredEdge from v1 to v2
    */
  def TransferredEdge(paths: PathMap, v1: Location, v2: Location, e1: Event, e2: Event): Boolean = {
    val p1 = NthDefault(e1.eiid, paths, List.empty[(Location, Location)])
    val p2 = NthDefault(e2.eiid, paths, List.empty[(Location, Location)])
    p1.contains((v1, v2)) && p2.contains((v1, v2))
  }

  /**
    * Given a set [candidates] of [LocalOrdering] edges at a vertex [v1], return the set of edges that are
    * [TransferredEdge]s from [v1] to [v2]; i.e., those such that the source and dest of the edge both have [v2] as the
    * next stage in their paths.
    * - path: intra instruction edges
    * - local ordering: ordering enforced with at a given location
    * - v2: location where we want to find out if local ordering enforced with at v1 are still maintained
    * @param paths paths corresponding to each event
    * @param candidates [LocalOrdering] edges at a vertex [v1]
    * @param v1 start vertex
    * @param v2 end vertex
    * @return ordering still maintained from v1 to v2
    */
  def TransferredEdges(paths: PathMap, candidates: List[(Event, Event)], v1: Location, v2: Location)
  : List[(Event, Event)] = {
    candidates match {
      case Nil => Nil
      case head :: next => 
        if (TransferredEdge(paths, v1, v2, head._1, head._2))
          (head._1, head._2) :: TransferredEdges(paths, next, v1, v2)
        else
          TransferredEdges(paths, next, v1, v2)
    }
  }

  /**
    * Given a set [candidates] of [LocalOrdering] edges at a vertex [v1], return the set of [TransferredEdges], and
    * apply the [local_reordering] that a location performs.
    * @param paths path map
    * @param candidates LocalOrdering edges at vertex v1
    * @param edgesAll the ordering as seen by all previous locations
    * @param v1 vertex where candidates reside
    * @param v2 the location which needs to calculate its local orderings
    * @param localReordering reordering performed by location v2
    * @return set of [TransferredEdges]
    */
  def TransferredReorderedEdges(paths: PathMap, candidates: List[(Event, Event)], edgesAll: List[List[(Event, Event)]],
                                v1: Location, v2: Location, localReordering: LocalReordering): List[(Event, Event)] = {
    localReordering(edgesAll)(TransferredEdges(paths, candidates, v1, v2))
  }

  /**
    * Return the union of the sets of [TransferredReorderedEdges] from every previous location to [v2].
    * compare to TransferredReorderedEdges, missing 2 parameters:
    * - candidates: local ordering -> replaced by first element in edgesAll
    * - v1: set to 0
    * @param paths path map
    * @param edgesAll the ordering as seen by all previous locations
    * @param v2 end location
    * @param localReordering reordering performed by location v2
    * @return union of the sets of [TransferredReorderedEdges] from every previous location to [v2]
    */
  def EdgesToTransferredEdges(paths: PathMap, edgesAll: List[List[(Event, Event)]], v2: Location,
                              localReordering: LocalReordering): List[(Event, Event)] = {
    /**
      * Return the set of [TransferredReorderedEdges] from [v1] to [v2], where [v1] is the index into the list of list
      * of edges [edgesAll].
      * compare to TransferredReorderedEdges, missing 1 parameter:
      * - candidates: local ordering -> replaced by first element in edgesAll
      * @param paths path map
      * @param edgesAll the ordering as seen by all previous locations
      * @param v1 start location
      * @param v2 end location
      * @param localReordering reordering performed by location v2
      * @return set of [TransferredReorderedEdges] from [v1] to [v2]
      */
    def helper(paths: PathMap, edgesAll: List[List[(Event, Event)]], v1: Location, v2: Location,
               localReordering: LocalReordering): List[List[(Event, Event)]] = {
      edgesAll match {
        case Nil => Nil
        case head :: next => TransferredReorderedEdges(paths, head, edgesAll, v1, v2, localReordering) ::
                             helper(paths, next, v1 + 1, v2, localReordering)
      }
    }

    helper(paths, edgesAll, 0, v2, localReordering).flatten
  }

  /**
    * Given a list of edges at locations 0 through (n - 1), calculate and append the list of edges at location n.
    * @param paths path map
    * @param localReordering reordering performed by location n
    * @param edgesAll list of edges at locations 0 through (n - 1)
    * @return the list of edges at location n appended at edgesAll
    */
  def EdgesToEdges(paths: PathMap, localReordering: LocalReordering, edgesAll: List[List[(Event, Event)]])
  : List[List[(Event, Event)]] = {
    edgesAll.appended(EdgesToTransferredEdges(paths, edgesAll, edgesAll.length, localReordering))
  }

  /**
    * Given a list of events originating at a given location, create program order edges between edges from the same
    * program order index.
    * @param l list of events originating at a given location
    * @return program order edges
    */
  def ProgramOrderEdges(l: List[Event]): List[(Event, Event)] = {
    /* sorted by processor index */
    val sortedEvents = l.foldLeft(List.empty[List[Event]])((_l, e) => AppendToNth(_l, e.iiid.proc, e))
    /* generate all program order edges (not sorted by processor ids in return value) */
    sortedEvents.flatMap(PathTransitiveClosure)
  }

  /**
    * Given a list of locations (as specified as a list of [LocalReordering]s), a set of LocalOrdering edges from the
    * first location, and a PathMap defining the Path taken by each Event in the scenario, calculate the full set of
    * LocalOrdering edges in the scenario.
    * IntraLocationEdges handles the base case, i.e., the stage which is defined in terms of program order, since there
    * are no previous locations.
    * Here local ordering edges are those with start location and end location in the same core.
    * @param paths the Path taken by each Event in the scenario
    * @param events events sorted by first location
    * @param localReorderings a list of locations
    * @return full set of local ordering edges in the scenario
    */
  def IntraLocationEdges(paths: PathMap, events: List[List[Event]], localReorderings: List[LocalReordering])
  : List[List[(Event, Event)]] = {
    /**
      * Given a list of locations (defined as a list of [LocalReordering]s), a set of [LocalOrdering] Edges from the
      * first location, and a PathMap defining the Path taken by each Event in the scenario, calculate the full set of
      * LocalOrdering edges in the scenario.
      * helper handles cases other than the first, i.e., those which are defined in terms of previous locations rather
      * than program order.
      * @param paths the Path taken by each Event in the scenario
      * @param edgesAll set of LocalOrdering edges (calculated one by one from location 0 to n)
      * @param poEvents events sorted by first location
      * @param localReorderings a list of locations
      * @return full set of local ordering edges in the scenario
      */
    @tailrec
    def helper(paths: PathMap, edgesAll: List[List[(Event, Event)]], poEvents: List[List[Event]],
               localReorderings: List[LocalReordering]): List[List[(Event, Event)]] = {
      (localReorderings, poEvents) match {
        case (oh :: ot, eh :: et) =>
          /* when there are events left, calculate their program order edges and append to edgesAll;
           * since the events are sorted by first location, this can be done along with calculating edges
           * before location 0 */
          helper(paths, AppendToLast(ProgramOrderEdges(eh), EdgesToEdges(paths, oh, edgesAll)), et, ot)
        case (oh :: ot, _) =>
          /* when there are no more events left (meaning that no more events starts at current location) in the poEvents
           * list, keep calculate EdgesToEdges until the last location in list of locations, a.k.a., localReorderings */
          helper(paths, EdgesToEdges(paths, oh, edgesAll), Nil, ot)
        case _ =>
          /* when local reordering list (locations) and event list all becomes nil, the calculation is done */
          edgesAll
      }
    }

    helper(paths, Nil, events, localReorderings)
  }

  /* Global Events */

  /**
    * A [GlobalEvent] is a memory [Event] (as its [eiid]) passing through a particular [location].
    */
  type GlobalEvent = (Location, Eiid)
  /**
    * A [GlobalGraph] is a list of labeled edges between [GlobalEvent]s
    */
  type GlobalGraph = List[(GlobalEvent, GlobalEvent, String)]

  def GlobalEventString(p: Pipeline, ge: GlobalEvent): String = {
    val (n, e) = ge
    (NthError(p.stages, n), n - p.stages.length) match {
      case (Some(s), _) => s"Event $e at ${s.name}"
      case (None, 0) => s"CacheLine $e Create"
      case (None, 1) => s"CacheLine $e Invalidate"
      case _ => "Unknown"
    }
  }

  def GraphString(g: List[(Int, Int, String)]): String = {
    g match {
      case head :: next =>
        val (s, d) = (head._1, head._2)
        s"${GraphString(next)}$s --${head._3}-> $d\n"
      case Nil => ""
    }
  }

  def GlobalGraphString(g: GlobalGraph): String = {
    g match {
      case head :: next =>
        val (s, d) = (head._1, head._2)
        s"${GlobalGraphString(next)}(${s._1}, ${s._2}) --${head._3}-> (${d._1}, ${d._2})\n"
      case Nil => ""
    }
  }

  /* Pipeline Model */

  /* Pipeline Stages */

  /**
    * A SpecialEdgeMap produces a set of extra edges from a given [Event] to edges coming either before or after it in
    * program order.
    * For example, a store buffer might ensure that only one unacknowledged store is outstanding at any given time;
    * in this case, we would add an global edge from the [GlobalEvent] of the first [Event] getting acknowledged to the
    * subsequent [Event] leaving the store buffer.
    */
  type SpecialEdgeMap = List[Event] => Event => List[Event] => GlobalGraph

  /**
    * A pipeline [Stage] is defined by its name, its numerical ID (which must be monotonically increasing), the
    * [LocalReordering] it performs, and a [SpecialEdgeMap] if applicable.
    * @param name name of pipeline stage
    * @param localReordering local reordering this stage performs
    * @param specialEdges a special edge map
    */
  case class Stage(name: String, localReordering: LocalReordering, specialEdges: SpecialEdgeMap)

  /**
    * A [PerformStages] specifies the locations at which an instruction performs with respect to each core along its
    * path. [observability] refers to the situation in which a particular performing location is only visible to stores
    * from certain cores.
    * For example, a read forwarded from the store buffer may perform with respect to all cores
    * when it performs, but it can only observe stores from the same core in this situation.
    * ---
    * Performing Location
    * A location [stage] is a performing location with respect to core [c] if:
    * - a load at location [stage] can read the value written by a store from core [c]
    * - the data being written by a store at location [stage] is visible to core [c]
    * ---
    * Considering that every [PerformStages] is related to a given event [evt] in [PathOption], for [evt], each
    * [PerformStages] records a location, where a core in [cores] can read the value stored by [evt] when [evt] passed
    * this location. In the meantime, [evt] can read value written by stores from core in list [cores] at location
    * [stage]. Thus, special case mentioned above means that, only the second condition is satisfied.
    * @param stage same as locations
    * @param cores list of cores with respect to which location [stage] is a performing location
    * @param observability cores from which location [stage] can observe stores
    * @param cacheLineInvLoc invalid cache line
    * @param isMainMemory if this is a main memory FIXME not used by now
    */
  case class PerformStages(stage: Int, cores: List[Int], observability: List[Int],
                           cacheLineInvLoc: Option[Int], isMainMemory: Boolean)

  /**
    * A [PathOption] is a possible [Path] for an [Event] through a given [Pipeline], together with a [SpecialEdgeMap]
    * that adds any extra orderings associated with the option.
    * For example, a cache miss may flush the pipeline, meaning that no subsequent (in program order) [Event]s will
    * leave the fetch stage until the cache miss response is received.
    * @param optionName name of this case
    * @param evt corresponding event of this optional path
    * @param path list of locations
    * @param performStages locations at which event [evt] performs with respect to other cores along its path
    * @param sem a special edge map
    */
  case class PathOption(optionName: String, evt: Event, path: Path,
                        performStages: List[PerformStages], sem: SpecialEdgeMap)

  /**
    * [PathOptions] represents a set of paths for a single event, such that only one will be chosen in any given
    * scenario.
    */
  type PathOptions = List[PathOption]

  /**
    * [Scenario] represents a set of paths for different events, such that the set of [PathOption]s taken together
    * represent the path of each [Event] in a scenario.
    */
  type Scenario = List[PathOption]

  /**
    * A [Pipeline] is defined as a set of [Stage]s, a function [pathsFor] that maps each event into a list of its
    * possible [PathOptions]
    * @param pipeName name of this pipeline
    * @param stages stages in this pipeline
    * @param pathsFor maps given event into a list of PathOptions
    */
  case class Pipeline(pipeName: String, stages: List[Stage], pathsFor: Event => PathOptions)

  /* Edges in the Global Ordering Graph */

  /**
    * Given an [Event] and the [Path] it takes, produce the associated set of global edges for that path.
    * @param e event
    * @param p the path e takes
    * @return associated set of global edges for that path
    */
  def IntraEventGlobalEdges(e: Event, p: Path): GlobalGraph = {
    p match {
      case ph :: pt => pt match {
        case Nil => Nil
        case th :: _ => /* ph and th makes an edge ([p|e]h -> y axis  eiid: x axis) */
          /* these are the vertical black edges in example uhb graph */
          ((ph, e.eiid), (th, e.eiid), "IntraEvent") :: IntraEventGlobalEdges(e, pt)
      }
      case Nil => Nil
    }
  }

  /**
    * Calculate the [IntraEventGlobalEdges] for each [Path] in a [Scenario].
    * @param l scenario
    * @return sum of intra event global edges for each event in given scenario
    */
  def ScenarioIntraEventGlobalEdges(l: Scenario): GlobalGraph = {
    l match {
      case Nil => Nil
      case PathOption(_, e, p, _, _) :: next => IntraEventGlobalEdges(e, p) ::: ScenarioIntraEventGlobalEdges(next)
    }
  }

  /**
    * Convert the list of local edges at each [location] in a [Scenario] [Pipeline] into the corresponding set of
    * global edges.
    * @param l location where [events in the first element of le] perform
    * @param le list of [list of local edges in given location]
    * @return corresponding set of global edges
    */
  def IntraLocationGlobalEdges(l: Location, le: List[List[(Event, Event)]]): GlobalGraph = {
    /**
      * Convert a list of local edges at a given [location] into the corresponding set of global edges.
      * @param l location
      * @param le list of local edges
      * @return corresponding set of global edges
      */
    def helper(l: Location, le: List[(Event, Event)]): GlobalGraph = {
      le match {
        case (e1, e2) :: next => ((l, e1.eiid), (l, e2.eiid), "IntraLocation") :: helper(l, next)
        case Nil => Nil
      }
    }

    le match {
      case head :: next => helper(l, head) ::: IntraLocationGlobalEdges(l + 1, next)
      case Nil => Nil
    }
  }

  /**
    * Given a set of events, sort them into a list of lists based on the first location in their paths
    * @param s scenario
    * @return events sorted by the first location in their paths
    */
  def EventsSortedByFirstLocation(s: Scenario): List[List[Event]] = {
    /**
      * Given a set of events, sort them into a list of lists based on the first location in their paths
      * @param l sorted part of given scenario
      * @param s scenario
      * @return events sorted by the first location in their paths
      */
    @tailrec
    def helper(l: List[List[Event]], s: Scenario): List[List[Event]] = {
      s match {
        case head :: next =>
          val e = head.evt
          val p = head.path
          p match {
            case h :: _ => helper(AppendToNth(l, h, e), next) /* h: first location in the path */
            case Nil => helper(l, next) /* path is nil */
          }
        case Nil => l
      }
    }

    helper(Nil, s)
  }

  /**
    * Convert the list of local edges at each location in a [Scenario] into the corresponding set of global edges.
    * @param s scenario (provide paths of each event)
    * @param pipeline pipeline (provide local reordering at each location)
    * @return set of global edges
    */
  def ScenarioIntraLocationGlobalEdges(s: Scenario, pipeline: Pipeline): GlobalGraph = {
    val pathPairs = s.map(/* get path for each event */_.path).map(/* get edges */PairConsecutive)
    val localReorderings = pipeline.stages.map(_.localReordering)
    IntraLocationGlobalEdges(0, IntraLocationEdges(pathPairs, EventsSortedByFirstLocation(s), localReorderings))
  }

  /**
    * Given a list [l] of [Event]s, return the list of [Event]s before [e] and after [e], with before and after
    * determined by program order index.
    * @param l list of events
    * @param e event used to partition list l
    * @return events before and after e
    */
  def PartitionAtEvent(l: List[Event], e: Event): (List[Event], List[Event]) = {
    l.filterNot(_.eiid == e.eiid).partition(_.iiid.poi < e.iiid.poi)
  }

  /**
    * Given a list l of Events, return the list of events that has the same processor id with e.
    * @param l list of events
    * @param e event
    * @return events in l that has the same processor id with e
    */
  def LocalEvents(l: List[Event], e: Event): List[Event] = {
    l.filter(_.iiid.proc == e.iiid.proc)
  }

  /**
    * Calculate the set of all special global edges due to the [Event] path choices in a given [Scenario]
    * @param s given scenario
    * @return all special global edges
    */
  def ScenarioPathSpecialEdges(s: Scenario): GlobalGraph = {
    /**
      * Calculate the set of all special global edges due to the [Event] path choices in a given [Scenario]
      * @param s PathOptions left in scenario
      * @param prevEvents save already handled events in scenario
      * @return all special global edges
      */
    def helper(s: Scenario, prevEvents: List[Event]): GlobalGraph = {
      s match {
        case Nil => Nil
        case PathOption(_, e, _, _, sem) :: next =>
          val localEvents = LocalEvents(/* all events in scenario s */prevEvents ::: s.map(_.evt), e)
          PartitionAtEvent(localEvents, e) match {
            case (before, after) => sem(before)(e)(after) ::: helper(next, prevEvents.appended(e))
          }
      }
    }

    helper(s, Nil)
  }

  /**
    * Calculate the set of all special global edges at a given [location]
    * @param e event
    * @param events list of event
    * @param m special edge map
    * @return all special global edges at a given [location]
    */
  def LocationSpecialEdges(e: Event, events: List[Event], m: SpecialEdgeMap): GlobalGraph = {
    val localEvents = LocalEvents(events, e)
    PartitionAtEvent(localEvents, e) match {
      case (before, after) => m(before)(e)(after)
    }
  }

  /**
    * Calculate the set of all special global edges due to the pipelines in a given [PathOption]
    * @param l list of locations
    * @param e event
    * @param events list of events
    * @param p pipeline
    * @return all special global edges
    */
  def PathPipelineSpecialEdges(l: List[Location], e: Event, events: List[Event], p: Pipeline): GlobalGraph = {
    l match {
      case Nil => Nil
      case head :: next =>
        val default: SpecialEdgeMap = _ => _ => _ => List.empty[(GlobalEvent, GlobalEvent, String)]
        val m = NthDefault(head, p.stages.map(_.specialEdges), default)
        LocationSpecialEdges(e, events, m) ::: PathPipelineSpecialEdges(next, e, events, p)
    }
  }

  /**
    * Calculate the set of all special global edges due to the pipelines in a given [Scenario]
    * @param s scenario
    * @param p pipeline
    * @return all special global edges
    */
  def ScenarioPipelineSpecialEdges(s: Scenario, p: Pipeline): GlobalGraph = {
    s match {
      case Nil => Nil
      case PathOption(_, evt, path, _, _) :: next =>
        PathPipelineSpecialEdges(path, evt, s.map(_.evt), p) ::: ScenarioPipelineSpecialEdges(next, p)
    }
  }

  /**
    * Calculate the set of all global edges in a [Scenario]
    * @param t title of graph
    * @param p pipeline
    * @param s scenario
    * @return all global edges in s
    */
  def ScenarioEdges(t: String, p: Pipeline, s: Scenario): GraphTree[GlobalEvent] = {
    val ms = new TinyTimer("ScenarioEdges")
    ms.reset() /* start timer */
    val result = GraphTreeLeaf(t,
      ScenarioIntraLocationGlobalEdges(s, p) :::
      ScenarioIntraEventGlobalEdges(s) :::
      ScenarioPipelineSpecialEdges(s, p) :::
      ScenarioPathSpecialEdges(s)
    )
    println(ms) /* stop timer */
    result
  }
}