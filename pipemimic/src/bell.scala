package pipemimic

object Bell {

  /* return unique values in l then append l's length at the end */
  def UniqueValues(l: List[Int]): List[Int] = {

    /* extract unique values from l to r */
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
      UniqueValues(l).map(l.appended(_))
    }
    if (n == 0) List(List.empty[Int])
    else BellNumber(n - 1).map(helper(_)).foldLeft(List.empty[List[Int]])((l, as) => l ::: as)
  }
}