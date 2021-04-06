package pipemimic

import java.io._

import org.scalatest.flatspec.AnyFlatSpec
import pipemimic.PreservedProgramOrder.GraphsToVerifyTSOPPO
import pipemimic.Stages._
import ListUtils._

import scala.annotation.tailrec

/** gem5 O3 Pipeline */
object Gem5Test {
  /* Local Reorderings at different stages */

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
  def Restore(n: Int): LocalReordering = e => _ => NthDefault(n, e, Nil)

  /* Special Edge Maps */

  /**
    * In some cases, we need to add certain extra edges to the graph to
    * capture non-local effects.
    */

  /**
    * In most cases, we don't need to add any special edges
    */
  val NoSpecialEdges: SpecialEdgeMap = _ => _ => _ => Nil

  /**
    * The store buffer only allows one outstanding unacknowledged store at a time
    */

  /**
    * Same Address
    * Load x -> Load x (Locally) : Speculative Load Reordering
    */
  def LoadCacheLineSpecialEdges(n: Int, c: Int)(eventsBefore: List[Event])(e: Event)(eventsAfter: List[Event]): GlobalGraph = {
    List(
      ((9 * c + 4, e.eiid), (9 * c + 5, e.eiid), "LoadCacheLine")
    )
  }
  
  def SpeculativeLoadReorderingSpecialEdges(n: Int, c: Int)(eventsBefore: List[Event])(e: Event)(eventsAfter: List[Event]): GlobalGraph = {
    eventsAfter match {
      case h :: t => (h.dirn, e.loc == h.loc) match {
        case (Direction.R, true) =>
          ((4 + 9 * c, e.eiid), (5 + 9 * c, h.eiid), "SLR") :: SpeculativeLoadReorderingSpecialEdges(n, c)(eventsBefore)(e)(t)
        case _ =>
          SpeculativeLoadReorderingSpecialEdges(n, c)(eventsBefore)(e)(t)
      }
      case Nil => Nil
    }
  }

  def LoadSpecialEdges(n: Int, c: Int)(eventsBefore: List[Event])(e: Event)(eventsAfter: List[Event]): GlobalGraph = {
    LoadCacheLineSpecialEdges(n, c)(eventsBefore)(e)(eventsAfter) :::
    SpeculativeLoadReorderingSpecialEdges(n, c)(eventsBefore)(e)(eventsAfter)
  }

  /**
   * Load x -> Store x (Locally) : Load checks for entires <= ID in store buffer 
   * Interpret FR edges as ''don't look at later stores''
   * 
   * Store x -> Load x (Locally) : Stores check for ordering violations 
   * When the store executes, squash the load if it already performed. Store set predictors are here too.
   */
  
  def StoreLoadSpecialEdges(n: Int, c: Int)(eventsBefore: List[Event])(e: Event)(eventsAfter: List[Event]): GlobalGraph = {
    eventsAfter match {
      case h :: t => (h.dirn, e.loc == h.loc) match {
        case (Direction.R, true) =>
          ((4 + 9 * c, e.eiid), (3 + 9 * c, h.eiid), "StoreLoad") :: StoreLoadSpecialEdges(n, c)(eventsBefore)(e)(t)
        case _ =>
          StoreLoadSpecialEdges(n, c)(eventsBefore)(e)(t)
      }
      case Nil => Nil
    }
  }

  /**
   * Store x -> Store x (Locally) : Store buffer ordered by pre-issue order
   * Performing location is rename, not execute!  SB slot reserved at rename!
   */

  /**
   * Different Address
   * ---
   * Load x -> Load y (Remotely) : Fails!
   * Load x -> Store y (Remotely) : Load -> In-order commit -> Store
   * Store x -> Store y (Remotely) : Store buffer is one at a time
   */

  @tailrec
  def StoreBufferSpecialEdges(n: Int, c: Int)(eventsBefore: List[Event])(e: Event)(eventsAfter: List[Event]): GlobalGraph = {
    eventsAfter match {
      case h :: t => h.dirn match {
        case Direction.R => StoreBufferSpecialEdges(n, c)(eventsBefore)(e)(t)
        case Direction.W => List(((9 * n + 1, e.eiid), (9 * c + 7, h.eiid), "StoreBuffer"))
      }
      case Nil => Nil
    }
  }

  /* Pipeline Definition */

  /**
    * Pipeline Stages
    * Each pipeline stage is defined by a name, a number, a [LocalReordering],
    * and a function adding any special edges.
    */
  def Gem5O3PipelineStages(n: Int, c: Int): List[Stage] = {
    List(
      Stage("Fetch", FIFO, NoSpecialEdges),
      Stage("Decode", FIFO, NoSpecialEdges),
      Stage("Rename", FIFO, NoSpecialEdges),
      Stage("Issue", NoOrderGuarantees, NoSpecialEdges),
      Stage("Execute", NoOrderGuarantees, NoSpecialEdges),
      Stage("CacheLineInvalidate", NoOrderGuarantees, NoSpecialEdges),
      Stage("Writeback", NoOrderGuarantees, NoSpecialEdges),
      Stage("Commit", Restore(2 + 9 * c), NoSpecialEdges),
      Stage("StoreBuffer", FIFO, StoreBufferSpecialEdges(n, c))
    )
  }

  val Gem5O3MemoryHierarchyStages = List(
    Stage("L2CacheForWrites", NoOrderGuarantees, NoSpecialEdges),
    Stage("Retire", NoOrderGuarantees, NoSpecialEdges),
  )

  def Gem5O3AllStages(n: Int): List[Stage] = {
    List.tabulate(n)(Gem5O3PipelineStages(n, _)).flatten ::: Gem5O3MemoryHierarchyStages
  }

  def StagesOfCore(c: Int, l: List[Int]): List[Int] = {
    l.map(_ + 9 * c)
  }

  /** Pipeline Paths */
  def Gem5O3PathOptions(n: Int, e: Event): List[PathOption] = {
    val c = e.iiid.proc
    e.dirn match {
      case Direction.R => List(
        PathOption(
          optionName = s"Read${e.loc}",
          evt = e,
          path = StagesOfCore(c, (0 to 4).toList) ::: StagesOfCore(c, (6 to 8).toList),
          performStages = List(
            PerformStages(4 + 9 * c, (0 until n).toList, (0 until n).toList, Some(5 + 9 * c), isMainMemory = true)
          ),
          sem = LoadSpecialEdges(n, c)
        )
      )
      case Direction.W => List(
        PathOption(
          optionName = s"Write${e.loc}",
          evt = e,
          path = StagesOfCore(c, (0 to 4).toList ::: StagesOfCore(c, (6 to 8).toList) ::: StagesOfCore(n, (0 to 1).toList)),
          performStages = List(
            PerformStages(2 + 9 * c, List(c), List(c), None, isMainMemory = false),
            PerformStages(9 * n, (0 until n).toList, (0 until n).toList, None, isMainMemory = true)
          ),
          sem = StoreLoadSpecialEdges(n, c)
        )
      )
    }

  }

  def Gem5O3Pipeline(n: Int): Pipeline = {
    Pipeline("Gem5O3", Gem5O3AllStages(n), Gem5O3PathOptions(n, _))
  }
}

class Gem5Test extends AnyFlatSpec {
  "it" should "pass all litmus tests" in {
    val allPPOGraphs = GraphsToVerifyTSOPPO(Gem5Test.Gem5O3Pipeline(1))
    println(s"found ${allPPOGraphs.length} ppo graphs")
    val allLitmusTestGraphs = Litmus.AllLitmusTests.map(t => t (Gem5Test.Gem5O3Pipeline(4)))
    println(s"found ${allLitmusTestGraphs.length} litmus test graphs")
    val allGraphs = allPPOGraphs ++ allLitmusTestGraphs.flatten

    val dots = allGraphs.map(_._2)
    val names = allGraphs.map(_._1.filter(_.isLetterOrDigit))
    dots zip names map {
      case (str, i) =>
        val writer = new PrintWriter(new File(s"./graphs/$i.gv"))
        writer.write(str)
        writer.close()
    }
  }
}
