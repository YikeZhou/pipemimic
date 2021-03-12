package pipemimic

object ListUtils {

  /* Cartesian Product of Lists of Lists */

  def CartesianProduct[T](l: List[List[T]]): List[List[T]] = {
    
    def helper0[T](h: T, t: List[List[T]]): List[List[T]] = {
      t match {
        case Nil => Nil
        case _ => t.map( h :: _ )
      }
    }

    def helper1[T](h: List[T], t: List[List[T]]): List[List[T]] = {
      h match {
        case head :: next => helper0(head, t) ++ helper1(next, t)
        case Nil => Nil
      }
    }

    l match {
      case Nil => Nil
      case List(h) => h.map(x => List(x))
      case head :: next => helper1(head, CartesianProduct(next))
    }
  }

  /* Add If Unique */

  def AddUnique[A](l: List[A], n: A): List[A] = {
    if (l.contains(n)) l else l.appended(n)
  }

  /* List of pairs of consecutive elements */

  def PairConsecutive[T](l: List[T]): List[Tuple2[T, T]] = {
    l match {
      case lh :: lt => {
        lt match {
          case th :: _ => (lh, th) :: PairConsecutive(lt)
          case _ => Nil
        }
      }
      case _ => Nil
    }
  }

  def PairConsecutiveWithLabel[T](n: String, l: List[T]): List[Tuple3[T, T, String]] = {
    l match {
      case lh :: lt => {
        lt match {
          case th :: _ => (lh, th, n) :: PairConsecutiveWithLabel(n, lt)
          case _ => Nil
        }
      }
      case _ => Nil
    }
  }

  /* Replace the [n]th element of [l] with [v].  If [l] is shorter than [n],
    fill in empty slots with [d]. */
  def replaceNth[T](l: List[T], n: Int, v: T, d: T) : List[T] = {
    require(n >= 0)

    if (l.length <= n) {
      val count = n - l.length
      (l ::: List.fill(count)(d)).appended(v)
    } else {
      l.updated(n, v)
    }
  }

  def replaceNthIfNone[T](l: List[Option[T]], n: Int, v: T): List[Option[T]] = {
    require(n >= 0)

    if (l.length <= n) (l ::: List.fill(n - l.length)(None)).appended(Some(v))
    else {
      val s = l.toSeq
      s(n) match {
        case None => l.updated(n, Some(v))
        case Some(_) => l
      }
    }
  }
  
  /* Append [a] to the [n]th sublist of [l], and create it if it doesn't already exist. */
  def AppendToNth[T](l: List[List[T]], n: Int, a: T): List[List[T]] = {
    require(n >= 0)

    l match {
      case head :: next => if (n > 0) head :: AppendToNth(next, n - 1, a) else (head.appended(a)) :: next
      case Nil => if (n > 0) List() :: AppendToNth(List(), n - 1, a) else List(List(a))
    }
  }

  def AppendUniqueToNth[T](l: List[List[T]], n: Int, a: T): List[List[T]] = {
    require(n >= 0)
    
    l match {
      case head :: next => if (n > 0) head :: AppendUniqueToNth(next, n - 1, a) else AddUnique[T](head, a) :: next
      case Nil => if (n > 0) List() :: AppendUniqueToNth(List(), n - 1, a) else List(List(a))
    }
  }

  def AppendToLast[T](x: List[T], l: List[List[T]]): List[List[T]] = {
    l match {
      case Nil => List(x)
      case List(y) => List(x ++ y)
      case head :: next => head :: AppendToLast(x, next)
    }
  }
}