package pipemimic

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import Interleavings._

class InterleavingsTest extends AnyFlatSpec {
  "Interleavings" should "return full interleavings of input lists" in {
    val l1 = List.range(1, 3)
    val l2 = List.range(3, 5)
    
    val r1 = List(
      1 :: 2 :: 3 :: 4 :: Nil,
      1 :: 3 :: 2 :: 4 :: Nil,
      1 :: 3 :: 4 :: 2 :: Nil,
      3 :: 1 :: 2 :: 4 :: Nil,
      3 :: 1 :: 4 :: 2 :: Nil,
      3 :: 4 :: 1 :: 2 :: Nil
    )

    assert(Interleave(l1 :: l2 :: Nil) == r1)
  }
}