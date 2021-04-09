package pipemimic.organization

class Interleaving[A](orders: List[A]*) {
  private val elementsCnt = orders.map(_.length).sum

  private def join(l: List[List[A]], r: List[List[A]]): List[(A, List[List[A]])] = {
    r match {
      case head :: next => head match {
        case h :: n => (h, l ::: (n :: next)) :: join(l.appended(head), next)
        case Nil => join(l, next)
      }
      case Nil => Nil
    }
  }

  private def prefix(head: A, tail: List[List[A]]): List[List[A]] = {
    tail match {
      case Nil => List(List(head))
      case _ => tail.map(head :: _)
    }
  }

  private def interleaves(ls: List[List[A]], n: Int): List[List[A]] = {
    require(n >= 0)
    if (n > 0) join(Nil, ls) flatMap { case (head, tail) => prefix(head, interleaves(tail, n - 1)) } else Nil
  }

  private val result = interleaves(orders.toList, elementsCnt)
}

object Interleaving {
  def apply[A](orders: List[A]*): List[List[A]] = {
    val interleaving = new Interleaving(orders: _*)
    interleaving.result
  }
}
