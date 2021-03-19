package pipemimic

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import Litmus.MatchingWrites

class MatchingWritesTest extends AnyFlatSpec {
  "MatchingWrites" should "pass" in {
    assert(MatchingWrites(Event(0, Iiid(0, 0), Access(Direction.R, 0, 0)), List(
      Event(1, Iiid(1, 0), Access(Direction.W, 0, 0)),
      Event(2, Iiid(1, 1), Access(Direction.W, 0, 1))
    )) == List(
      (Some(Event(1, Iiid(1, 0), Access(Direction.W, 0, 0))), Event(0, Iiid(0, 0), Access(Direction.R, 0, 0))),
      (None, Event(0, Iiid(0, 0), Access(Direction.R, 0, 0)))
    ))
  }
}
