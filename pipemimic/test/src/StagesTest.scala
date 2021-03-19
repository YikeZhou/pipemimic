package pipemimic

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import Stages._
import ListUtils._
import PreservedProgramOrder.GraphsToVerifyPPOWithAnyAddresses
import Execution.GraphsToVerifyExecution

class StagesTest extends AnyFlatSpec {
  "PathTC" should "return transitive closure of a given path" in {
    val path = List(1, 2, 3)
    val tc = List((1, 2), (1, 3), (2, 3))
    assert(PathTransitiveClosure(path) == tc)
  }

  "Stages & PPO & Execution" should "work well" in {
    val e0 = Event(0, Iiid(0, 0), Access(Direction.W, 0, 1))
    val e1 = Event(1, Iiid(0, 1), Access(Direction.R, 0, 0))
    val e2 = Event(2, Iiid(0, 2), Access(Direction.R, 0, 0))
    val e3 = Event(3, Iiid(0, 3), Access(Direction.W, 0, 1))

    val events = List(e0, e1, e2, e3)

    val pathPairs: PathMap = List(
      List((0, 1), (1, 3)),
      List((0, 1), (1, 3)),
      List((0, 2), (2, 3)),
      List((0, 2), (2, 3))
    )

    val edges0 = List(PathTransitiveClosure(events))

    val FIFO: LocalReordering = _ => z => z

    val edges01 = EdgesToEdges(pathPairs, FIFO, edges0)

    assert(edges01 == edges0 ::: List(List((e0, e1)))) // EdgesExample

    val edges012 = EdgesToEdges(pathPairs, FIFO, edges01)
    val edges0123 = EdgesToEdges(pathPairs, FIFO, edges012)

    val myLocalReorderings = List.fill(4)(FIFO)

    assert(IntraLocationEdges(pathPairs, List(events), myLocalReorderings) == edges0123) // EdgesExample2

    val NoSpecialEdges: SpecialEdgeMap = _ => _ => _ => Nil

    val myStages = List(
      Stage("F", FIFO, NoSpecialEdges),
      Stage("FL", FIFO, NoSpecialEdges),
      Stage("FS", FIFO, NoSpecialEdges),
      Stage("L", FIFO, NoSpecialEdges),
      Stage("S", FIFO, NoSpecialEdges),
      Stage("LC", FIFO, NoSpecialEdges),
      Stage("SC", FIFO, NoSpecialEdges),
      Stage("C", FIFO, NoSpecialEdges),
    )

    def myPathsFor(e: Event) = {
      e.dirn match {
        case Direction.R => PathOption("Read", e, List(0, 1, 3, 5, 7), List(PerformStages(3, List(0), List(0), None, true)), NoSpecialEdges) :: Nil
        case Direction.W => PathOption("Write", e, List(0, 2, 4, 6, 7), List(PerformStages(4, List(0), List(0), None, true)), NoSpecialEdges) :: Nil
      }
    }

    val myPipeline = Pipeline("SamplePipeline", myStages, myPathsFor)

    val myScenarios = List(List(
      PathOption("Write", e0, List(0, 2, 4, 6, 7), List(PerformStages(4, List(0), List(0), None, true)), NoSpecialEdges),
      PathOption("Read", e1, List(0, 1, 3, 5, 7), List(PerformStages(3, List(0), List(0), None, true)), NoSpecialEdges),
      PathOption("Read", e2, List(0, 1, 3, 5, 7), List(PerformStages(3, List(0), List(0), None, true)), NoSpecialEdges),
      PathOption("Write", e3, List(0, 2, 4, 6, 7), List(PerformStages(4, List(0), List(0), None, true)), NoSpecialEdges)
    ))

    val myScenario: Scenario = Head(Nil, myScenarios)

    val myGlobalEdges = ScenarioEdges("my_sample", myPipeline, myScenario)

    val leaf = GraphTreeLeaf("my_sample", List(
      /* location 0 */
      ((0, 0), (0, 1), "IntraLocation"), ((0, 0), (0, 2), "IntraLocation"),
      ((0, 0), (0, 3), "IntraLocation"), ((0, 1), (0, 2), "IntraLocation"),
      ((0, 1), (0, 3), "IntraLocation"), ((0, 2), (0, 3), "IntraLocation"),
      /* location 1 */
      ((1, 1), (1, 2), "IntraLocation"),
      /* location 2 */
      ((2, 0), (2, 3), "IntraLocation"),
      /* location 3 */
      ((3, 1), (3, 2), "IntraLocation"),
      /* location 4 */
      ((4, 0), (4, 3), "IntraLocation"),
      /* location 5 */
      ((5, 1), (5, 2), "IntraLocation"),
      /* location 6 */
      ((6, 0), (6, 3), "IntraLocation"),
      /* location 7 */
      ((7, 1), (7, 2), "IntraLocation"), ((7, 0), (7, 3), "IntraLocation"),
      /* event 0 */
      ((0, 0), (2, 0), "IntraEvent"), ((2, 0), (4, 0), "IntraEvent"),
      ((4, 0), (6, 0), "IntraEvent"), ((6, 0), (7, 0), "IntraEvent"),
      /* event 1 */
      ((0, 1), (1, 1), "IntraEvent"), ((1, 1), (3, 1), "IntraEvent"),
      ((3, 1), (5, 1), "IntraEvent"), ((5, 1), (7, 1), "IntraEvent"),
      /* event 2 */
      ((0, 2), (1, 2), "IntraEvent"), ((1, 2), (3, 2), "IntraEvent"),
      ((3, 2), (5, 2), "IntraEvent"), ((5, 2), (7, 2), "IntraEvent"),
      /* event 3 */
      ((0, 3), (2, 3), "IntraEvent"), ((2, 3), (4, 3), "IntraEvent"),
      ((4, 3), (6, 3), "IntraEvent"), ((6, 3), (7, 3), "IntraEvent")
    ))

    assert(myGlobalEdges == leaf) // EdgesExample3

    val evt00 = Event(0, Iiid(0, 0), Access(Direction.W, 0, 1))
    val evt10 = Event(1, Iiid(1, 0), Access(Direction.R, 0, 1))

    println("Start sample validation execution")
    val sampleValidationExecution = GraphsToVerifyExecution("Sample", myPipeline, List(evt00, evt10), List((0, 1)))
    println(sampleValidationExecution) // EdgesExample5

    /* FIXME ppo test */
    println("Start ppo checking")
    val WRRW = List(Direction.W, Direction.R, Direction.R, Direction.W)

    val sampleValidationPPO = GraphsToVerifyPPOWithAnyAddresses(myPipeline, WRRW, 0, 3)
    println("Finish sample validation")
    println(sampleValidationPPO)
  }

  "EventsSortedByFirstLocation" should "sort correctly" in {
    val e0 = Event(0, Iiid(0, 0), Access(Direction.W, 0, 1))
    val e1 = Event(1, Iiid(0, 1), Access(Direction.W, 0, 2))
    val e2 = Event(2, Iiid(2, 0), Access(Direction.W, 0, 3))

    val events = List(e0, e1, e2)

    val NoSpecialEdges: SpecialEdgeMap = _ => _ => _ => Nil

    def DummyScenario(e: Event): PathOption = PathOption("Dummy", e, List(1, 2), List.empty[PerformStages], NoSpecialEdges)

    assert(EventsSortedByFirstLocation(events.map(DummyScenario(_))) == List(Nil, events))
  }
}
