package object pipemimic {
  
  /* Basic Types */

  type Word = Int /* ignore word-size and alignment issues for now */
  type Address = Word /* memory addresses */
  type Value = Word /* values stored in memory */

  type Processor = Int /* Processors are indexed by natural numbers. */
  type ProgramOrderIndex = Int /* index in program order */

  case class Iiid(proc: Processor, poi: ProgramOrderIndex) /* instruction instance id */
  type Eiid = Int /* event instance id */

  object Direction extends Enumeration {
    val R, W = Value /* read or write */
  }

  abstract class Action /* an access specified by polarity + location + value */
  case class Access(d: Direction.Value, l: Address, v: Value) extends Action

  /**
    * Each instance of an instruction in a program execution may
    * give rise to multiple events, as described by the instruction
    * semantics. Events may be individual reads and writes to 
    * memory, and memory barrier events.
    *
    * @param eiid an unique identifier
    * @param iiid the associated index in program order and the processor
    * @param action the associated action
    */
  case class Event(eiid: Eiid, iiid: Iiid, action: Action) {
    def dirn: Direction.Value = {
      this.action match {
        case Access(d, _, _) => d
      }
    }

    def loc: Address = {
      this.action match {
        case Access(_, l, _) => l
      }
    }

    def isWrite: Boolean = this.dirn == Direction.W
  }

  /** list methods FIXME: keep this as simple as possible */
  implicit class ListImprovements[A](val l: List[A]) {
    def tailError: List[A] = if (l.isEmpty) l else l.tail
    def nthDefault(n: Int, default: A): A = l.lift(n).getOrElse(default)
    def pairConsecutive: List[(A, A)] = l.init zip l.tail
    def pairConsecutive(label: String): List[(A, A, String)] = l.pairConsecutive.map(t => (t._1, t._2, label))
    def addUnique(n: A): List[A] = if (l.contains(n)) l else l.appended(n)
    def replaceNth(n: Int, v: A, d: A): List[A] =
      if (l.length <= n) List.concat(l, List.fill(n - l.length)(d).appended(v)) else l.updated(n, v)
  }

  implicit class OptionListImprovements[A](val l: List[Option[A]]) {
    def replaceNthIfNone(n: Int, v: A): List[Option[A]] = {
      if (l.length <= n)
        List.concat(l, List.fill(n - l.length)(None), List(Some(v)))
      else if (l(n).isEmpty)
        l.updated(n, Some(v))
      else
        l
    }
  }

  implicit class TwoDimensionalListImprovements[A](val l: List[List[A]]) {
    def appendToNth(n: Int, a: A, isUnique: Boolean = false): List[List[A]] = {
      if (l.length <= n)
        List.concat(l, List.fill(n - l.length)(List.empty), List(List(a)))
      else {
        l.zipWithIndex map {
          case (value, i) =>
            if (i != n) value else if (i == n && isUnique) value.addUnique(a) else value.appended(a)
        }
      }
    }
    def appendToLast(x: List[A]): List[List[A]] = l.init.appended(l.last ::: x)
  }
}