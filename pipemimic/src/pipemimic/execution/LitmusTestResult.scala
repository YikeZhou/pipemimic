package pipemimic.execution

import pipemimic.statistics.DotGraph

case class LitmusTestResult(observable: Boolean, observed: List[DotGraph], unobserved: List[DotGraph], casesCnt: Int)
