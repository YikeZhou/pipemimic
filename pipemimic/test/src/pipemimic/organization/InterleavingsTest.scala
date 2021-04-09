package pipemimic.organization

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

class InterleavingsTest extends AnyFlatSpec {
  "Interleavings" should "return full interleavings of input lists" in {
    val l1 = List.range(1, 3) /* 1, 2 */
    val l2 = List.range(3, 5) /* 3, 4 */
    
    val r1 = List(
      1 :: 2 :: 3 :: 4 :: Nil,
      1 :: 3 :: 2 :: 4 :: Nil,
      1 :: 3 :: 4 :: 2 :: Nil,
      3 :: 1 :: 2 :: 4 :: Nil,
      3 :: 1 :: 4 :: 2 :: Nil,
      3 :: 4 :: 1 :: 2 :: Nil
    )

    assert(Interleaving(l1, l2) == r1)
  }
}