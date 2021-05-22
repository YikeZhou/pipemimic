package pipemimic

import pipemimic.execution.{LitmusTest, LitmusTestConstructor}
import pipemimic.pipeline.PipelineFactory
import pipemimic.ppo.{AnyAddress, SameAddress}
import pipemimic.statistics.{DotGraph, TinyTimer}

import java.io.{File, FileWriter, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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

object PreservedProgramOrderDebug extends App {

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
}

object LitmusTestDebug extends App {
  require(args.length >= 2)

  val pipelineName = args.head
  val outputDirectory = s"./graphs/${args.head}"

  /* construct a pipeline (using arg[0]) */
  val pipelineFactory = new PipelineFactory
  val constructor = pipelineFactory.createPipeline(pipelineName)

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

object PreservedProgramOrderRelease extends App {
  val writer = new FileWriter("profiling/po-result.csv", true)
  val profiler = new FileWriter("profiling/po-profiling.csv", true)

  val pipelines = Seq("WR", "rWR", "rWM", "rMM")
  val orders = ppo.ProgramOrder.values
//  for (order <- orders) println(order)

  // output header line into csv file
  val header = new StringBuilder("arch")
  for (po <- orders) {
    header.append(',')
    header.append(
      po match {
        case ppo.ProgramOrder.ReadAfterRead => "rr"
        case ppo.ProgramOrder.ReadAfterWrite => "wr"
        case ppo.ProgramOrder.WriteAfterRead => "rw"
        case ppo.ProgramOrder.WriteAfterWrite => "ww"
      }
    )
  }
  writer.write(header.toString() + '\n')
  profiler.write(header.toString() + '\n')

  writer.write("# Any Address\n")
  profiler.write("# Any Address\n")

  for (pipelineName <- pipelines) {
    println("**********************************************")
    println(pipelineName + " - PPO Any Address Testing")
    println("**********************************************")
    val pipelineFactory = new PipelineFactory
    val constructor = pipelineFactory.createPipeline(pipelineName)
    val ppoPipeline = constructor.pipelineWithCore(1)

    writer.write(pipelineName + ",")
    profiler.write(pipelineName + ",")

    /* 1-2 any address */
    val res = ArrayBuffer.empty[String]
    val times = ArrayBuffer.empty[String]

    val anyAddressTester = new AnyAddress(ppoPipeline)
    /* warm up */
    for (po <- orders) new AnyAddress(ppoPipeline).isSatisfied(po)

    for (po <- orders) {
      val runtime = TinyTimer(po.toString + pipelineName)
      println(s"[Any Address] $po is satisfied: ${anyAddressTester.isSatisfied(po)}")
      println(runtime)
      times.addOne(runtime.timeElapsed.toString)

      if (anyAddressTester.isSatisfied(po))
        res.addOne("y")
      else
        res.addOne("n")
    }
    writer.write(res.mkString(",") + '\n')
    profiler.write(times.mkString(",") + '\n')
  }

  writer.write("# Same Address\n")
  profiler.write("# Same Address\n")

  for (pipelineName <- pipelines) {
    println("**********************************************")
    println(pipelineName + " - PPO Same Address Testing")
    println("**********************************************")
    val pipelineFactory = new PipelineFactory
    val constructor = pipelineFactory.createPipeline(pipelineName)
    val ppoPipeline = constructor.pipelineWithCore(1)

    writer.write(pipelineName + ",")
    profiler.write(pipelineName + ",")

    /* 1-2 any address */
    val res = ArrayBuffer.empty[String]
    val times = ArrayBuffer.empty[String]

    val sameAddressTester = new SameAddress(ppoPipeline)
    /* warm up */
    for (po <- orders) new SameAddress(ppoPipeline).isSatisfied(po)

    for (po <- orders) {
      val runtime = TinyTimer(po.toString + pipelineName)
      println(s"[Same Address] $po is satisfied: ${sameAddressTester.isSatisfied(po)}")

      if (sameAddressTester.isSatisfied(po))
        res.addOne("y")
      else
        res.addOne("n")
      println(runtime)
      times.addOne(runtime.timeElapsed.toString)
    }
    writer.write(res.mkString(",") + '\n')
    profiler.write(times.mkString(",") + '\n')
  }

  writer.close()
  profiler.close()
}

object LitmusTestRelease extends App {
  val writer = new FileWriter("profiling/litmus-result.csv", true)
  val profiler = new FileWriter("profiling/litmus-profiling.csv", true)

  /* construct pipeline objects */
  val pipelines = Seq("WR", "rWR", "rWM", "rMM")
  val processors = pipelines map { p =>
    val pipelineFactory = new PipelineFactory
    val constructor = pipelineFactory.createPipeline(p)
    constructor.pipelineWithCore(2)
  }

  writer.write("test," + pipelines.mkString(",") + '\n')
  profiler.write("test," + pipelines.mkString(",") + '\n')

  for (arg <- args) {
    val testName = arg.substring(57, arg.indexOf(".litmus"))
    val res = ArrayBuffer.empty[String]
    val times = ArrayBuffer.empty[String]
    res.addOne(testName)
    times.addOne(testName)

    /* warm up: load class into memory */
    for (arch <- processors)
      LitmusTestConstructor(arg).getResults(arch).observable

    for (arch <- processors) {
      val runtime = TinyTimer(testName) // ms
      if (LitmusTestConstructor(arg).getResults(arch).observable)
        /* equivalent */ res.addOne("eq")
      else
        /* stricter */ res.addOne("st")
      println(runtime)
      /* save runtime into profiling csv file */
      times.addOne(runtime.timeElapsed.toString)
    }

    writer.write(res.mkString(",") + '\n')
    profiler.write(times.mkString(",") + '\n')
  }

  writer.close()
  profiler.close()
}