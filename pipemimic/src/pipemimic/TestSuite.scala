package pipemimic

import pipemimic.execution.{LitmusTest, LitmusTestConstructor}
import pipemimic.pipeline.PipelineFactory
import pipemimic.ppo.{AnyAddress, SameAddress}
import pipemimic.statistics.DotGraph

import scala.collection.mutable.ListBuffer

object TestSuite extends App {
  require(args.length >= 2)

  val pipelineName = args.head
  val outputDirectory = s"./graphs/${args.head}"

  /* construct a pipeline (using arg[0]) */
  val pipelineFactory = new PipelineFactory
  val constructor = pipelineFactory.createPipeline(pipelineName)

  /* 1. ppo check for all type of program order */
  val ppoPipeline = constructor.pipelineWithCore(1)

  /* 1-1 same address */
  val sameAddressTester = new SameAddress(ppoPipeline)
  for (po <- ppo.ProgramOrder.values) {
    println(s"[Same Address] $po is satisfied: ${sameAddressTester.isSatisfied(po)}")
    sameAddressTester.getGraphs(po).foreach(_.write(outputDirectory + "/sameAddr"))
  }

  /* 1-2 any address */
  val anyAddressTester = new AnyAddress(ppoPipeline)
  for (po <- ppo.ProgramOrder.values) {
    println(s"[Any Address] $po is satisfied: ${anyAddressTester.isSatisfied(po)}")
    anyAddressTester.getGraphs(po).foreach(_.write(outputDirectory + "/anyAddr"))
  }

  /*-------------------------------------------------------------------------------------------*/

  /* 2. litmus test for all 36 tests in litmus-tests-riscv/tests/non-mixed-size/BASIC_2_THREAD */
  val litmusPipeline = constructor.pipelineWithCore(2)

  /* build a list of litmus tests */
  val litmusTestsForRVWMO = ListBuffer.empty[LitmusTest]

  for (arg <- args.tail) /* read all test file specified in args[1:] */
    litmusTestsForRVWMO += LitmusTestConstructor(arg)

  val allLitmusTestGraphs = ListBuffer.empty[DotGraph]

  for (litmusTest <- litmusTestsForRVWMO) {
    val result = litmusTest.getResults(litmusPipeline)
    // TODO save uhb graph
    allLitmusTestGraphs.appendAll(result.unobserved).appendAll(result.observed)
    assert(result.casesCnt == result.observed.length + result.unobserved.length)
    println(s"observable = ${result.observable} after checking ${result.casesCnt} cases")
  }

  println(s"found ${allLitmusTestGraphs.length} litmus test results")

  val dots = allLitmusTestGraphs.toList
  dots foreach (_.write(outputDirectory + "/litmus"))
}
