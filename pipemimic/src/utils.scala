package pipemimic

import Stages.{Pipeline, GlobalEvent, GlobalGraph}

object ListUtils {

  /* Get certain element of a list */

  def LastError[A](l: List[A]): Option[A] = {
    if (l.isEmpty) None else Some(l.last)
  }

  def Head[T](default: T, l: List[T]): T = {
    if (l.isEmpty) default else l.head
  }

  def Tail[T](x: List[T]): List[T] = {
    if (x.isEmpty) x else x.tail
  }

  def NthDefault[T](n: Int, l: List[T], default: T): T = {
    require(n >= 0)
    l.lift(n) match {
      case None => default
      case Some(value) => value
    }
  }

  def NthError[T](l: List[T], n: Int): Option[T] = {
    (n, l) match {
      case (0, head :: _) => Some(head)
      case (n, _ :: next) if n > 0 => NthError(l, n)
      case _ => None
    }
  }

  /* List of pairs of consecutive elements */

  def PairConsecutive[A](l: List[A]): List[(A, A)] = {
    l match {
      case Nil => Nil
      case lh :: lt => lt match {
        case Nil => Nil
        case th :: tt => (lh, th) :: PairConsecutive(lt)
      }
    }
  }

  def PairConsecutiveWithLabel[A](label: String, l: List[A]): List[(A, A, String)] = {
    l match {
      case Nil => Nil
      case lh :: lt => lt match {
        case Nil => Nil
        case th :: tt => (lh, th, label) :: PairConsecutiveWithLabel(label, lt)
      }
    }
  }

  /* Add If Unique */

  def AddUnique[A](l: List[A], n: A): List[A] = {
    if (l.contains(n)) l else l.appended(n)
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

object CartesianUtils {

  /* Cartesian Product of Lists of Lists */

  def CartesianProduct[T](l: List[List[T]]): List[List[T]] = {
    /**
      * Given a head element [h] and a list of tail lists [t], generate
      * the list of lists with [h] prepended to each list in [t].
      *
      * @param h head element
      * @param t list of tail lists
      * @return list of lists with `h` prepended to each list in `t`
      */
    def prependOne[T](h: T, t: List[List[T]]): List[List[T]] = t.map( h :: _ )

    /**
      * Given a list of head elements [h] and a list of tail lists [t],
      * generate the list of lists with each element of [h] prepended to
      * each list in [t].
      *
      * @param h list of head elements
      * @param t list of tail lists
      * @return list of lists with each element of `h` prepended to each list in `t`
      */
    def prependList[T](h: List[T], t: List[List[T]]): List[List[T]] = {
      h match {
        case head :: next => prependOne(head, t) ++ prependList(next, t)
        case Nil => Nil
      }
    }

    l match {
      case Nil => Nil
      case List(h) => h.map(x => List(x))
      case head :: next => prependList(head, CartesianProduct(next))
    }
  }

  /* Cartesian Product of Two Lists as Pairs */

  def CartesianProductPairs[A, B](h: List[A], t: List[B]): List[(A, B)] = {
    def helper[A, B](h: A, t: List[B]): List[(A, B)] = t.map((h, _))
    h.map(helper(_, t)).foldLeft(List.empty[(A, B)])(_ ++ _)
  }
}

class TinyTimer(name: String) {
  var start: Long = _
  var init = false

  def reset = {
    init = true
    start = System.nanoTime()
  }

  override def toString(): String = {
    val timeElapsed = (System.nanoTime() - start) / 1000000
    if (init) s"Timer<$name>: $timeElapsed ms" else "Error: not initialized"
  }
}

object GlobalGraphIDUtils {
  def geid(p: Pipeline, ge: GlobalEvent): Int = {
    ge match {
      case (n, e) => e * p.stages.length + n
    }
  }

  def gepid(p: Pipeline, gep: (GlobalEvent, GlobalEvent, String)): (Int, Int, String) = {
    (geid(p, gep._1), geid(p, gep._2), gep._3)
  }

  def getid(p: Pipeline, t: GraphTree[GlobalEvent]): GraphTree[Int] = {
    t match {
      case GraphTreeOr(l) => GraphTreeOr(l.map(getid(p, _)))
      case GraphTreeAnd(l) => GraphTreeAnd(l.map(getid(p, _)))
      case GraphTreeLeaf(s, l) => GraphTreeLeaf(s, l.map(gepid(p, _)))
    }
  }

  def ungeid(p: Pipeline, n: Int): (Stages.Location, ProgramOrderIndex) = {
    def helper(p: Pipeline, n: Int, s: Int, e: Int): (Stages.Location, ProgramOrderIndex) = {
      if (s == p.stages.length) {
        if (n == 0) (0, e + 1) else helper(p, n - 1, 1, e + 1)
      } else if (s < p.stages.length) {
        if (n == 0) (s, e) else helper(p, n - 1, s + 1, e)
      } else {
        /* ERROR */ (0, 0)
      }
    }
    
    helper(p, n, 0, 0)
  }
}