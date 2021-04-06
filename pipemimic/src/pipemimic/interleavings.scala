package pipemimic

object Interleavings {
  /* Return a list of all possible sequential interleavings of the input lists */
  def Interleave[T](l: List[List[T]]): List[List[T]] = {
    
    def helper0[A](l1: List[List[A]], l2: List[List[A]]): List[(A, List[List[A]])] = {
      l2 match {
        case head2 :: next2 =>
          head2 match {
            case head :: next => (head, l1 ++ (next :: next2)) :: helper0(l1.appended(head2), next2)
            case Nil => helper0(l1, next2)
          }
        case Nil => Nil
      }
    }

    def helper1[B](a: B, l: List[List[B]]): List[List[B]] = {
      l match {
        case Nil => List(List(a))
        case _ => l.map(x => a :: x)
      }
    }

    def helper2[C](l: List[List[C]], n: Int): List[List[C]] = {
      require(n >= 0)

      if (n > 0) {
        helper0(List(), l).map(p => helper1(p._1, helper2(p._2, n - 1))).foldLeft(List.empty[List[C]])( _ ++ _ )
      } else Nil
    }

    val n = l.map(_.length).sum
    helper2(l, n)
  }
}