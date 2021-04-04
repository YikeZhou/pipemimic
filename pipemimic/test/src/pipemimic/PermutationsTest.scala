package pipemimic

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import Litmus.Permutations

class PermutationsTest extends AnyFlatSpec {
  "Permutations" should "calculate permutations list" in {
    assert(
      Permutations(List(1, 2, 3)) ==
      List(
        List(1, 2, 3),
        List(2, 1, 3),
        List(2, 3, 1),
        List(1, 3, 2),
        List(3, 1, 2),
        List(3, 2, 1)
      )
    )

    assert(Permutations(List(1, 2, 3, 4, 5)).length == 120)
  }
}
