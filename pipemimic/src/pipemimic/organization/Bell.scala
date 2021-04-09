package pipemimic.organization

class Bell(n: Int) {
  require(n >= 0)

  /* return unique values in l then append l's length at the end */
  private def uniqueValues(l: List[Int]) = {
    val s = l.toSet
    s.toList.appended(s.size)
  }

  /* return nth bell number */
  def numbers: List[List[Int]] = {
    val helper: List[Int] => List[List[Int]] = l => uniqueValues(l).map(l.appended)
    if (n == 0) List(Nil)
    else Bell(n - 1).flatMap(helper)
  }
}

object Bell {
  def apply(n: Int): List[List[Int]] = {
    val bell = new Bell(n)
    bell.numbers
  }
}
