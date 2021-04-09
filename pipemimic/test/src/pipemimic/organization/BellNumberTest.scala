package pipemimic.organization

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class BellNumberTest extends AnyFlatSpec {
  
//  "UniqueValues" should "return unique values in a list along with list length" in {
//    val round = 10
//    val maxLength = 20
//
//    for (_ <- 0 until round) {
//      val length = Random.nextInt() % maxLength + 1
//      val l = (0 until length).toList
//      val result = l.toSet
//
//      assert(UniqueValues(l) == l.appended(l.length))
//    }
//  }
  
  "Bell" should "return correct bell numbers" in {
    val bells = Seq(1, 1, 2, 5, 15, 52, 203)    
    for (i <- 0 until 7) assert(Bell(i).length == bells(i))
  }
}
