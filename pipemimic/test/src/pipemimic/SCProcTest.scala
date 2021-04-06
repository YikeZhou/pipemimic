package pipemimic

import java.io._

import org.scalatest.flatspec.AnyFlatSpec
import pipemimic.PreservedProgramOrder.GraphsToVerifyTSOPPO
import pipemimic.Stages._

/** Traditional Five-Stage SCProc Pipeline */
object SCProcTest {
  /* Local Reorderings at different stages */

  /**
    * any ordering guaranteed at the input is also guaranteed at the output. This is the common case.
    */
  val FIFO: LocalReordering = _ => ordering => ordering

  /**
    * Operations can leave the stage in any order; nothing is guaranteed.
    */
  val NoOrderGuarantees: LocalReordering = _ => _ => Nil

  /* Special Edge Maps */

  /**
    * In most cases, we don't need to add any special edges
    */
  val NoSpecialEdges: SpecialEdgeMap = _ => _ => _ => Nil

  /* Pipeline Definition */

  /**
    * Pipeline Stages
    * Each pipeline stage is defined by a name, a number, a [LocalReordering],
    * and a function adding any special edges (in this case, only at the store buffer).
    */
  val SCProcPipelineStages: List[Stage] = {
    List(
      Stage("Fetch", FIFO, NoSpecialEdges),
      Stage("Decode", FIFO, NoSpecialEdges),
      Stage("Execute", FIFO, NoSpecialEdges),
      Stage("Memory", FIFO, NoSpecialEdges),
      Stage("WriteBack", FIFO, NoSpecialEdges)
    )
  }

  def SCProcAllStages(n: Int): List[Stage] = {
    List.fill(n)(SCProcPipelineStages).flatten
  }

  def StagesOfCore(c: Int, l: List[Int]): List[Int] = {
    l.map(_ + 5 * c)
  }

  /** Pipeline Paths */
  def SCProcPathOptions(n: Int, e: Event): List[PathOption] = {
    val c = e.iiid.proc
    e.dirn match {
      case Direction.R => List(
        PathOption(
          optionName = s"Read${e.loc}",
          evt = e,
          path = StagesOfCore(c, (0 to 4).toList),
          performStages = List(
            PerformStages(3 + 5 * c, (0 until n).toList, (0 until n).toList, None, isMainMemory = true)
          ),
          sem = NoSpecialEdges
        )
      )
      case Direction.W => List(
        PathOption(
          optionName = s"Write${e.loc}",
          evt = e,
          path = StagesOfCore(c, (0 to 4).toList),
          performStages = List(
            PerformStages(3 + 5 * c, (0 until n).toList, (0 until n).toList, None, isMainMemory = true),
          ),
          sem = NoSpecialEdges
        )
      )
    }

  }

  def SCProcPipeline(n: Int): Pipeline = {
    Pipeline("SCProc", SCProcAllStages(n), SCProcPathOptions(n, _))
  }
}

class SCProcTest extends AnyFlatSpec {
  "it" should "pass all litmus tests" in {
    val allPPOGraphs = GraphsToVerifyTSOPPO(SCProcTest.SCProcPipeline(1))
    println(s"found ${allPPOGraphs.length} ppo graphs")
    val allLitmusTestGraphs = Litmus.AllLitmusTests.map(t => t (SCProcTest.SCProcPipeline(4)))
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