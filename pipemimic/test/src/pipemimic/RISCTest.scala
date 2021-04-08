package pipemimic

import pipemimic.PreservedProgramOrder.GraphsToVerifyTSOPPO
import java.io._

import org.scalatest.flatspec.AnyFlatSpec

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
        case Direction.R => StoreBufferSpecialEdges(c, n)(eventBefore)(e)(t)
        case Direction.W => List(((6 * n, e.eiid), (5 + 6 * c, h.eiid), "StoreBuffer"))
      }
      case Nil => Nil
    }
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
      case Direction.R => List(
        PathOption(s"Read${e.loc}", e,
          StagesOfCore(c, (0 to 4).toList),
          List(PerformStages(3 + 6 * c, (0 until n).toList, (0 until n).toList, None, isMainMemory = true)),
          NoSpecialEdges),
        PathOption(s"STBFwd${e.loc}", e,
          StagesOfCore(c, (0 to 4).toList),
          List(PerformStages(3 + 6 * c, (0 until n).toList, List(c), None, isMainMemory = false)),
          NoSpecialEdges)
      )
      case Direction.W => List(
        PathOption(
          optionName = s"Write${e.loc}",
          evt = e,
          path = StagesOfCore(c, (0 to 5).toList) ::: StagesOfCore(n, List(0, 1)),
          performStages = List(
            PerformStages(3 + 6 * c, List(c), List(c), None, isMainMemory = false),
            PerformStages(6 * n, (0 until n).toList, (0 until n).toList, None, isMainMemory = true)
          ),
          sem = NoSpecialEdges
        )
      )
    }
  }

  def RISCPipeline(n: Int): Pipeline = Pipeline("RISC", RISCAllStages(n), RISCPathOptions(n, _))
}

class RISCTest extends AnyFlatSpec {
  "it" should "print all graphs" in {
    val allPPOGraphs = GraphsToVerifyTSOPPO(RISCTest.RISCPipeline(1))
    println(s"found ${allPPOGraphs.length} ppo graphs")
    val allLitmusTestGraphs = Litmus.AllLitmusTests.map(t => t (RISCTest.RISCPipeline(4)))
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