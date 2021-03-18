package pipemimic

import ListUtils._

object Stages {
  type Location = Int /* Location in Local Ordering Graphs */
  type Path = List[Location] /* through which an operation passes during its execution */

  /* Transitive Closure of a [Path] */

  /**
    * Pair src with each element of dsts
    *
    * @param src first element of the tuples in return list
    * @param dsts including second elements of the tuples
    * @return list of tuples
    */
  def PathTCPair[A](src: A, dsts: List[A]): List[(A, A)] = {
    dsts match {
      case head :: next => (src, head) :: PathTCPair(src, next)
      case Nil => Nil
    }
  }

  /**
    * Given a path, generate the list of pairs representing the transitive
    * closure of the path
    *
    * @param l a path
    * @return transitive closure of path l
    */
  def PathTransitiveClosure[A](l: List[A]): List[(A, A)] = {
    l match {
      case head :: next => PathTCPair(head, next) ::: PathTransitiveClosure(next)
      case Nil => Nil
    }
  }

  type PathMap = List[List[(Location, Location)]] /* the nth element is the path taken by the nth event in program order */
  
  /* Graphs of Happens-Before Orderings */

  type LocalOrdering = List[(Event, Event)] /* an ordering on Events that is enforced with at a given location */
  type LocalReordering = List[LocalOrdering] => LocalOrdering => LocalOrdering

  def TransferredEdge(paths: PathMap, v1: Location, v2: Location, e1: Event, e2: Event): Boolean = {
    val p1 = NthDefault(e1.eiid, paths, List.empty[(Location, Location)])
    val p2 = NthDefault(e2.eiid, paths, List.empty[(Location, Location)])
    p1.contains((v1, v2)) && p2.contains((v1, v2))
  }

  def TransferredEdges(paths: PathMap, candidates: List[(Event, Event)], v1: Location, v2: Location): List[(Event, Event)] = {
    candidates match {
      case Nil => Nil
      case head :: next => 
        if (TransferredEdge(paths, v1, v2, head._1, head._2)) (head._1, head._2) :: TransferredEdges(paths, next, v1, v2) 
        else TransferredEdges(paths, next, v1, v2)
    }
  }

  def TransferredReorderedEdges(paths: PathMap, candidates: List[(Event, Event)], edgesAll: List[List[(Event, Event)]], v1: Location, v2: Location, localReordering: LocalReordering): List[(Event, Event)] = {
    localReordering(edgesAll)(TransferredEdges(paths, candidates, v1, v2))
  }

  def EdgesToTransferredEdges(paths: PathMap, edgesAll: List[List[(Event, Event)]], v2: Location, localReordering: LocalReordering): List[(Event, Event)] = {
    def helper(paths: PathMap, edgesAll: List[List[(Event, Event)]], v1: Location, v2: Location, localReordering: LocalReordering): List[List[(Event, Event)]] = {
      edgesAll match {
        case Nil => Nil
        case head :: next => TransferredReorderedEdges(paths, head, edgesAll, v1, v2, localReordering) ::
                             helper(paths, next, v1 + 1, v2, localReordering)
      }
    }

    helper(paths, edgesAll, 0, v2, localReordering).foldLeft(List.empty[(Event, Event)])( _ ::: _ )
  }

  def EdgesToEdges(paths: PathMap, localReordering: LocalReordering, edgesAll: List[List[(Event, Event)]]): List[List[(Event, Event)]] = {
    edgesAll.appended(EdgesToTransferredEdges(paths, edgesAll, edgesAll.length, localReordering))
  }

  def ProgramOrderEdges(l: List[Event]): List[(Event, Event)] = {
    val sortedEvents = l.foldLeft(List.empty[List[Event]])((_l, e) => AppendToNth(_l, e.iiid.proc, e))
    val edges = sortedEvents.map(PathTransitiveClosure(_))
    edges.foldLeft(List.empty[(Event, Event)])( _ ::: _ )
  }

  def IntraLocationEdges(paths: PathMap, events: List[List[Event]], localReorderings: List[LocalReordering]): List[List[(Event, Event)]] = {
    def helper(paths: PathMap, edgesAll: List[List[(Event, Event)]], poEvents: List[List[Event]], localReorderings: List[LocalReordering]): List[List[(Event, Event)]] = {
      (localReorderings, poEvents) match {
        case (oh :: ot, eh :: et) => helper(paths, AppendToLast(ProgramOrderEdges(eh), EdgesToEdges(paths, oh, edgesAll)), et, ot)
        case (oh :: ot, _) => helper(paths, EdgesToEdges(paths, oh, edgesAll), Nil, ot)
        case _ => edgesAll
      }
    }

    helper(paths, Nil, events, localReorderings)
  }

  /* Global Events */

  type GlobalEvent = (Location, Eiid)
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
      case head :: next => {
        val (s, d) = (head._1, head._2)
        s"${GraphString(next)}$s --${head._3}-> $d\n"
      }
      case Nil => ""
    }
  }

  def GlobalGraphString(g: GlobalGraph): String = {
    g match {
      case head :: next => {
        val (s, d) = (head._1, head._2)
        s"${GlobalGraphString(next)}(${s._1}, ${s._2}) --${head._3}-> (${d._1}, ${d._2})\n"
      }
      case Nil => ""
    }
  }

  /* Pipeline Model */

  /* Pipeline Stages */

  type SpecialEdgeMap = List[Event] => Event => List[Event] => GlobalGraph

  case class Stage(name: String, localReordering: LocalReordering, specialEdges: SpecialEdgeMap)

  case class PerformStages(stage: Int, cores: List[Int], observability: List[Int], cacheLineInvLoc: Option[Int], isMainMemory: Boolean)

  case class PathOption(optionName: String, evt: Event, path: Path, performStages: List[PerformStages], sem: SpecialEdgeMap)

  type PathOptions = List[PathOption]

  type Scenario = List[PathOption]

  case class Pipeline(pipeName: String, stages: List[Stage], pathsFor: Event => PathOptions)

  def IntraEventGlobalEdges(e: Event, p: Path): GlobalGraph = {
    p match {
      case ph :: pt => pt match {
        case Nil => Nil
        case th :: _ => ((ph, e.eiid), (th, e.eiid), "IntraEvent") :: IntraEventGlobalEdges(e, pt)
      }
      case Nil => Nil
    }
  }

  def ScenarioIntraEventGlobalEdges(l: Scenario): GlobalGraph = {
    l match {
      case Nil => Nil
      case PathOption(_, e, p, _, sem) :: next => IntraEventGlobalEdges(e, p) ::: ScenarioIntraEventGlobalEdges(next)
    }
  }

  def IntraLocationGlobalEdges(l: Location, le: List[List[(Event, Event)]]): GlobalGraph = {
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

  def EventsSortedByFirstLocation(s: Scenario): List[List[Event]] = {
    def helper(l: List[List[Event]], s: Scenario): List[List[Event]] = {
      s match {
        case head :: next => {
          val e = head.evt
          val p = head.path
          p match {
            case h :: _ => helper(AppendToNth(l, h, e), next)
            case Nil => helper(l, next)
          }
        }
        case Nil => l
      }
    }

    helper(Nil, s)
  }

  def ScenarioIntraLocationGlobalEdges(s: Scenario, pipeline: Pipeline): GlobalGraph = {
    val pathPairs = s.map(_.path).map(PairConsecutive(_))
    val localReorderings = pipeline.stages.map(_.localReordering)
    IntraLocationGlobalEdges(0, IntraLocationEdges(pathPairs, EventsSortedByFirstLocation(s), localReorderings))
  }

  def PartitionAtEvent(l: List[Event], e: Event): (List[Event], List[Event]) = {
    def helper(h: List[Event], t: List[Event], e: Event): (List[Event], List[Event]) = {
      t match {
        case Nil => (Nil, Nil)
        case head :: next => if (e.eiid == head.eiid) (h, next) else helper(h.appended(head), next, e)
      }
    }

    helper(Nil, l, e)
  }

  def LocalEvents(l: List[Event], e: Event): List[Event] = {
    l match {
      case Nil => Nil
      case head :: next => if (head.iiid.proc == e.iiid.proc) head :: LocalEvents(next, e) else LocalEvents(next, e)
    }
  }

  def ScenarioPathSpecialEdges(s: Scenario): GlobalGraph = {
    def helper(s: Scenario, prevEvents: List[Event]): GlobalGraph = {
      s match {
        case Nil => Nil
        case PathOption(_, e, p, _, sem) :: next => {
          val localEvents = LocalEvents(prevEvents ::: s.map(_.evt), e)
          PartitionAtEvent(localEvents, e) match {
            case (before, after) => sem(before)(e)(after) ::: helper(next, prevEvents.appended(e))
          }
        }
      }
    }

    helper(s, Nil)
  }

  def LocationSpecialEdges(e: Event, events: List[Event], m: SpecialEdgeMap): GlobalGraph = {
    val localEvents = LocalEvents(events, e)
    PartitionAtEvent(localEvents, e) match {
      case (before, after) => m(before)(e)(after)
    }
  }

  def PathPipelineSpecialEdges(l: List[Location], e: Event, events: List[Event], p: Pipeline): GlobalGraph = {
    l match {
      case Nil => Nil
      case head :: next => {
        val default: SpecialEdgeMap = _ => _ => _ => List.empty[(GlobalEvent, GlobalEvent, String)]
        val m = NthDefault(head, p.stages.map(_.specialEdges), default)
        LocationSpecialEdges(e, events, m) ::: PathPipelineSpecialEdges(next, e, events, p)
      }
    }
  }

  def ScenarioPipelineSpecialEdges(s: Scenario, p: Pipeline): GlobalGraph = {
    s match {
      case Nil => Nil
      case head :: next => PathPipelineSpecialEdges(head.path, head.evt, s.map(_.evt), p) ::: ScenarioPipelineSpecialEdges(next, p)
    }
  }

  def ScenarioEdges(t: String, p: Pipeline, s: Scenario): GraphTree[GlobalEvent] = {
    val ms = new TinyTimer("ScenarioEdges")
    ms.reset /* start timer */
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