package pipemimic

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import CartesianUtils.CartesianProduct

class CartesianProductTest extends AnyFlatSpec {
  "CartesianProduct" should "return the cartesian product of lists" in {
    val ll1 = List(List(0, 1), List(2), List(3, 4))
    val ll1r = List(List(0, 2, 3), List(0, 2, 4), List(1, 2, 3), List(1, 2, 4))

    val ll2 = List(List.empty[Int])
    val ll2r = List.empty[Int]

    val ll3 = List(List.empty[Int], List(0, 1))
    val ll3r = List.empty[Int]

    val ll4 = List(List(0, 1), List.empty[Int])
    val ll4r = List.empty[Int]
    
    val in = Seq(ll1, ll2, ll3, ll4)
    val out = Seq(ll1r, ll2r, ll3r, ll4r)

    for (i <- 0 until 4) assert(CartesianProduct(in(i)) == out(i))
  }
}