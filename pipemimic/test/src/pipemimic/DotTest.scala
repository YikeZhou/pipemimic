package pipemimic

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import Dot._

class DotTest extends AnyFlatSpec {
  "FormatString" should "drop empty string in list" in {
    assert(FormatString(List("", "")).isEmpty)
    assert(FormatString(List("a")) == "[a]")
    assert(FormatString(List("a", "")) == "[a]")
    assert(FormatString(List("a", "b")) == "[a,b]")
  }

  "DotGraph" should "export correct dot graph" in {
    val e0 = Event(0, Iiid(0, 0), Access(Direction.R, 0, 0))
    val e1 = Event(1, Iiid(0, 1), Access(Direction.R, 1, 0))

    val myGraph = List(
      /* GlobalEvent */
      ((0, e0), (0, e1), "a"),
      ((0, e0), (1, e0), "b")
    )

    def myNString(n: Int): String = {
      def helper(n: Int, e: Int): String = {
        (e, n) match {
          case (_e, _) if _e >= 10 => "Overflow"
          case (_, _n) if n >= 5 => helper(n - 5, e + 1)
          case _ => s"Event${e}AtLocation$n"
        }
      }

      helper(n, 0)
    }

    def myGeid(ne: (Int, Event)): Int = {
      val (n, e) = ne
      e.eiid * 5 + n
    }

    def myGepid(nep: ((Int, Event), (Int, Event), String)): (Int, Int, String) = {
      val (ne1, ne2) = (nep._1, nep._2)
      (myGeid(ne1), myGeid(ne2), nep._3)
      /* Hint: use gepid to convert GraphTree[GlobalEvent] to GraphTree[Int] */
    }

    def myUngeid(g: Int): (Int, Int) = {
      if (g >= 5) {
        val (n, e) = myUngeid(g - 5)
        (n, e + 1)
      } else {
        (g, 0)
      }
    }

    println(DotGraph("sample", myGraph.map(myGepid(_)), myUngeid, myNString, Nil, Nil, 5))
  }
}
