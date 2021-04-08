package pipemimic

import java.io.{File, PrintWriter}

import pipemimic.Stages.{GlobalGraph, LocalReordering, PathOption, PathOptions, PerformStages, Pipeline, SpecialEdgeMap, Stage}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object RISCTest {
  val FIFO: LocalReordering = _ => ordering => ordering

  val NoOrderGuarantees: LocalReordering = _ => _ => Nil

  val NoSpecialEdges: SpecialEdgeMap = _ => _ => _ => Nil

  @tailrec
  def StoreBufferSpecialEdges(c: Int, n: Int)(eventBefore: List[Event])(e: Event)(eventAfter: List[Event]): GlobalGraph = {
    eventAfter match {
      case h :: t => h.dirn match {
        case Some(Direction.R) => StoreBufferSpecialEdges(c, n)(eventBefore)(e)(t)
        case Some(Direction.W) => List(((6 * n, e.eiid), (5 + 6 * c, h.eiid), "StoreBuffer"))
        case _ => StoreBufferSpecialEdges(c, n)(eventBefore)(e)(t)
      }
      case Nil => Nil
    }
  }

  def FenceTSOSpecialEdges(c: Int, n: Int)(eventBefore: List[Event])(e: Event)(eventAfter: List[Event]): GlobalGraph = {
    require(e.action.isInstanceOf[MemoryFence])
    val edges = ListBuffer.empty[((Stages.Location, Eiid), (Stages.Location, Eiid), String)]
    eventBefore foreach {
      case Event(eiid, Iiid(proc, _), action) => action match {

        case Access(Direction.R, _, _) =>
          /* happens before all events in eventsAfter */
          for (latter <- eventAfter) {
            if (latter.dirn.contains(Direction.W))
              edges += (((6 * proc + 3, eiid), (6 * n, latter.eiid), "FenceTSO"))
            else if (latter.dirn.contains(Direction.R))
              edges += (((6 * proc + 3, eiid), (6 * proc + 3, latter.eiid), "FenceTSO"))
          }

        case Access(Direction.W, _, _) =>
          /* happens before all store events in eventsAfter */
          for (latter <- eventAfter if latter.dirn.contains(Direction.W))
            edges += (((6 * n, eiid), (6 * n, latter.eiid), "FenceTSO"))

        case MemoryFence() =>
      }
    }
    edges.toList
  }

  def RISCPipelineStages(n: Int, c: Int): List[Stage] = {
    List(
      Stage("Fetch", FIFO, NoSpecialEdges),
      Stage("Decode", FIFO, NoSpecialEdges),
      Stage("Execute", FIFO, NoSpecialEdges),
      Stage("Memory", FIFO, NoSpecialEdges),
      Stage("WriteBack", FIFO, NoSpecialEdges),
      Stage("StoreBuffer", FIFO, StoreBufferSpecialEdges(c, n)),
    )
  }

  def RISCSharedStages: List[Stage] = {
    List(
      Stage("MainMemory", NoOrderGuarantees, NoSpecialEdges),
      Stage("Retire", FIFO, NoSpecialEdges)
    )
  }

  def RISCAllStages(n: Int): List[Stage] = {
    List.tabulate(n)(RISCPipelineStages(n, _)).flatten ::: RISCSharedStages
  }

  def StagesOfCore(c: Int, l: List[Int]): List[Int] = l.map(_ + 6 * c)

  /* Pipeline Paths */

  def RISCPathOptions(n: Int, e: Event): PathOptions = {
    val c = e.iiid.proc
    e.dirn match {
      case Some(Direction.R) => List(
        PathOption(s"Read${e.loc.get}", e,
          StagesOfCore(c, (0 to 4).toList),
          List(PerformStages(3 + 6 * c, (0 until n).toList, (0 until n).toList, None, isMainMemory = true)),
          NoSpecialEdges),
        PathOption(s"STBFwd${e.loc.get}", e,
          StagesOfCore(c, (0 to 4).toList),
          List(PerformStages(3 + 6 * c, (0 until n).toList, List(c), None, isMainMemory = false)),
          NoSpecialEdges)
      )
      case Some(Direction.W) => List(
        PathOption(
          optionName = s"Write${e.loc.get}",
          evt = e,
          path = StagesOfCore(c, (0 to 5).toList) ::: StagesOfCore(n, List(0, 1)),
          performStages = List(
            PerformStages(3 + 6 * c, List(c), List(c), None, isMainMemory = false),
            PerformStages(6 * n, (0 until n).toList, (0 until n).toList, None, isMainMemory = true)
          ),
          sem = NoSpecialEdges
        )
      )
      case None => List(
        PathOption(
          optionName = s"Fence",
          evt = e,
          path = StagesOfCore(c, (0 to 4).toList),
          performStages = Nil,
          sem = FenceTSOSpecialEdges(c, n)
        )
      )
    }
  }

  def RISCPipeline(n: Int): Pipeline = Pipeline("RISC", RISCAllStages(n), RISCPathOptions(n, _))
}

object TestSuite extends App {
  /* build a list of litmus tests */
  val litmusTestsForRVWMO = ListBuffer.empty[LitmusTest.testFunc]

  for (arg <- args)
    litmusTestsForRVWMO += LitmusTest(arg)

  val allLitmusTestGraphs = litmusTestsForRVWMO.toList.flatMap(t => t(pipemimic.RISCTest.RISCPipeline(2)))
  println(s"found ${allLitmusTestGraphs.length} litmus test graphs")

  val dots = allLitmusTestGraphs.map(_._2)
  val names = allLitmusTestGraphs.map(_._1.filter(_.isLetterOrDigit))
  dots zip names map {
    case (str, i) =>
      val writer = new PrintWriter(new File(s"./graphs/$i.gv"))
      writer.write(str)
      writer.close()
  }
}
