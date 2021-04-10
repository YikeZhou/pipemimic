package pipemimic.execution

import pipemimic._

trait ScenariosForEvents {
  /**
    * Given pipeline and a list of events, calculate paths for all events and their names
    * @param p pipeline
    * @param events list of events (in litmus test)
    * @return all possible scenarios
    */
  def getScenarios(p: Pipeline, events: List[Event]): List[(String, Scenario)] = {

    def scenarioTitle(l: List[PathOption]) = l.map(_.optionName).mkString("->")

    val pathsForEvent = p.pathsFor(_)
    val allPaths = events.map(pathsForEvent)
    CartesianProduct(allPaths).map(scenario => (scenarioTitle(scenario), scenario))
  }
}
