package pipemimic

import scala.annotation.tailrec

object Bell {

  /* return unique values in l then append l's length at the end */
  def UniqueValues(l: List[Int]): List[Int] = {

    /* extract unique values from l to r */
    @tailrec
    def helper(l: List[Int], r: List[Int]): List[Int] = {
      l match {
        case Nil => r
        case head :: next => helper(next, ListUtils.AddUnique(r, head))
      }
    }

    val r = helper(l, List())
    r.appended(r.length)
  }

  /* return nth bell number */
  def BellNumber(n: Int): List[List[Int]] = {
    require(n >= 0)

    def helper(l: List[Int]): List[List[Int]] = {
      UniqueValues(l).map(l.appended)
    }
    if (n == 0) List(List.empty[Int])
    else BellNumber(n - 1).map(helper).foldLeft(List.empty[List[Int]])((l, as) => l ::: as)
  }
}