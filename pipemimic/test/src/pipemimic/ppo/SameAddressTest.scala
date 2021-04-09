package pipemimic.ppo

import org.scalatest.flatspec.AnyFlatSpec
import pipemimic.RISCTest

class SameAddressTest extends AnyFlatSpec {
  behavior of "SameAddress"

  it should "yield correct dot graph" in {
    val p = RISCTest.RISCPipeline(1)
    val sa = new SameAddress(p)
    for (po <- ProgramOrder.values) {
      println(sa.isSatisfied(po) + po.toString)
      sa.getGraphs(po).foreach(_.write("./graphs", po.toString))
    }
  }

}
