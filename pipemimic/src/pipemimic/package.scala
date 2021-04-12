package object pipemimic {
  
  /* Basic Types */

  /** ignore word-size and alignment issues for now */
  type Word = Int
  /** memory addresses */
  type Address = Word
  /** values stored in memory */
  type Value = Word

  /** Processors are indexed by natural numbers. */
  type Processor = Int
  /** index in program order */
  type ProgramOrderIndex = Int

  /** instruction instance id */
  case class Iiid(proc: Processor, poi: ProgramOrderIndex)
  /** event instance id */
  type Eiid = Int

  /** read or write */
  object Direction extends Enumeration {
    val R, W = Value
  }

  sealed abstract class Action
  /** individual reads and writes to memory */
  case class Access(direction: Direction.Value, address: Address, value: Value) extends Action
  /** memory barrier events: fence rw,rw (FenceTSO) */
  case class MemoryFence() extends Action

  /**
    * Each instance of an instruction in a program execution may
    * give rise to multiple events, as described by the instruction
    * semantics. Events may be individual reads and writes to 
    * memory, and memory barrier events.
    *
    * @param eiid an unique identifier
    * @param iiid the associated index in program order and the processor
    * @param action the associated action
    */
  case class Event(eiid: Eiid, iiid: Iiid, action: Action) {
    def dirn: Option[Direction.Value] = {
      this.action match {
        case Access(d, _, _) => Some(d)
        case MemoryFence() => None
      }
    }

    def addr: Option[Address] = {
      this.action match {
        case Access(_, addr, _) => Some(addr)
        case MemoryFence() => None
      }
    }

    def isWrite: Boolean = {
      val direction = this.dirn
      direction.isDefined && direction.get == Direction.W
    }

    def isRead: Boolean = {
      val direction = this.dirn
      direction.isDefined && direction.get == Direction.R
    }
  }

  /* Methods added to basic collections class */

  /** general list */
  implicit class ListImprovements[A](val l: List[A]) {
    def tailError: List[A] = if (l.isEmpty) l else l.tail
    def nthDefault(n: Int, default: A): A = l.lift(n).getOrElse(default)
    def pairConsecutive: List[(A, A)] = l.init zip l.tail
    def pairConsecutive(label: String): List[(A, A, String)] = l.pairConsecutive.map(t => (t._1, t._2, label))
    def addUnique(n: A): List[A] = if (l.contains(n)) l else l.appended(n)
    def replaceNth(n: Int, v: A, d: A): List[A] =
      if (l.length <= n) List.concat(l, List.fill(n - l.length)(d).appended(v)) else l.updated(n, v)
  }

  /** list of options */
  implicit class OptionListImprovements[A](val l: List[Option[A]]) {
    def replaceNthIfNone(n: Int, v: A): List[Option[A]] = {
      if (l.length <= n)
        List.concat(l, List.fill(n - l.length)(None), List(Some(v)))
      else if (l(n).isEmpty)
        l.updated(n, Some(v))
      else
        l
    }
  }

  /** list of list */
  implicit class TwoDimensionalListImprovements[A](val l: List[List[A]]) {
    def appendToNth(n: Int, a: A, isUnique: Boolean = false): List[List[A]] = {
      if (l.length <= n)
        List.concat(l, List.fill(n - l.length)(List.empty), List(List(a)))
      else {
        l.zipWithIndex map {
          case (value, i) =>
            if (i != n) value else if (i == n && isUnique) value.addUnique(a) else value.appended(a)
        }
      }
    }
    def appendToLast(x: List[A]): List[List[A]] = l.init.appended(l.last ::: x)
  }

  /* Local Ordering Graphs */

  /** Location in Local Ordering Graphs */
  type Location = Int

  /**
    * A [Path] is a list of locations (e.g., pipeline stages), given by [nat] indices, through which an operation passes
    * during its execution.
    */
  type Path = List[Location]

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
    (p.stages.lift(n), n - p.stages.length) match {
      case (Some(s), _) => s"Event${e}at${s.name}"
      case (None, 0) => s"CacheLine${e}Create"
      case (None, 1) => s"CacheLine${e}Invalidate"
      case _ => "Unknown"
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
    */
  abstract class Pipeline {
    /** described basic features of this pipeline */
    val pipeName: String
    /** all stages of this pipeline */
    val stages: List[Stage]
    /** map given event into list of its possible path options */
    val pathsFor: Event => PathOptions
    /** number of cores (currently only support 1 or 2) */
    val coreNumber: Int
    /** number of intra-core stages */
    val inCoreStageNumber: Int
    /** number of off-core stages (such as main memory or shared cache) */
    val unCoreStageNumber: Int
    /** given core id and index in local intra-core stages, return index in [[stages]] */
    def stageOfCore(core: Int, localStageIndex: Int): Int = {
      require(0 <= localStageIndex && localStageIndex < inCoreStageNumber + unCoreStageNumber)
      if (localStageIndex < inCoreStageNumber)
        core * inCoreStageNumber + localStageIndex
      else
        (localStageIndex - inCoreStageNumber) + coreNumber * inCoreStageNumber
    }
    /** given core id and indices in local intra-core stages, return indices in [[stages]] */
    def stageOfCore(core: Int, localStageIndices: Seq[Int]): List[Int] =
      localStageIndices.map(stageOfCore(core, _)).toList
  }

  /* Cartesian Product of Lists of Lists */

  def CartesianProduct[A](l: List[List[A]]): List[List[A]] = {
    /**
      * Given a head element [h] and a list of tail lists [t], generate
      * the list of lists with [h] prepended to each list in [t].
      *
      * @param h head element
      * @param t list of tail lists
      * @return list of lists with `h` prepended to each list in `t`
      */
    def prependOne(h: A, t: List[List[A]]): List[List[A]] = t.map( h :: _ )

    /**
      * Given a list of head elements [h] and a list of tail lists [t],
      * generate the list of lists with each element of [h] prepended to
      * each list in [t].
      *
      * @param h list of head elements
      * @param t list of tail lists
      * @return list of lists with each element of `h` prepended to each list in `t`
      */
    def prependList(h: List[A], t: List[List[A]]): List[List[A]] = {
      h match {
        case head :: next => prependOne(head, t) ++ prependList(next, t)
        case Nil => Nil
      }
    }

    l match {
      case Nil => Nil
      case List(h) => h.map(x => List(x))
      case head :: next => prependList(head, CartesianProduct(next))
    }
  }

  /* Cartesian Product of Two Lists as Pairs */

  def CartesianProduct[A, B](h: List[A], t: List[B]): List[(A, B)] = {
    def helper(h: A, t: List[B]): List[(A, B)] = t.map((h, _))
    h.flatMap(helper(_, t))
  }

  /* Litmus Test Constants */

  object LitmusTestExpectation extends Enumeration {
    val Forbidden, Permitted = Value
  }
}