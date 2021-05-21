package pipemimic.ppo

import org.scalatest.flatspec.AnyFlatSpec
import pipemimic.pipeline.PipelineFactory

class SameAddressTest extends AnyFlatSpec {
  behavior of "SameAddress"

  it should "yield correct dot graph" in {
    val f = new PipelineFactory
    val constructor = f.createPipeline("WR")
    val p = constructor.pipelineWithCore(1)
    val sa = new SameAddress(p)
    for (po <- ProgramOrder.values) {
      println(sa.isSatisfied(po) + po.toString)
      sa.getGraphs(po).foreach(_.write("./graphs", po.toString))
    }
  }

}
