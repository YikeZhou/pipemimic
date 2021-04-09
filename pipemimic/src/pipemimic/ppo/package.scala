package pipemimic

package object ppo {
  /* four type of program orders */
  object ProgramOrder extends Enumeration {
    val WriteAfterWrite, ReadAfterWrite, ReadAfterRead, WriteAfterRead = Value
  }

  val programOrderToDirectionsList = Map(
    ProgramOrder.WriteAfterWrite -> List(Direction.W, Direction.W),
    ProgramOrder.ReadAfterWrite -> List(Direction.W, Direction.R),
    ProgramOrder.ReadAfterRead -> List(Direction.R, Direction.R),
    ProgramOrder.WriteAfterRead -> List(Direction.R, Direction.W)
  )

  def getEvents(po: ProgramOrder.Value, atSameAddress: Boolean): List[List[Event]] = {
    val dirs = programOrderToDirectionsList(po)
    getEvents(dirs, atSameAddress)
  }

  def getEvents(po: List[Direction.Value], atSameAddress: Boolean): List[List[Event]] = {
    val blankEvents = po.zipWithIndex map { case (direction, index) =>
      Event(index, Iiid(0, index), Access(direction, 0, 0))
    }
    val poEvents = if (atSameAddress) {
      List(blankEvents)
    } else {
      /* assign address to events */
      Bell.BellNumber(blankEvents.length) map { locs =>
        /* locs: one possible overlapping for address */
        locs zip blankEvents map {
          case (location, Event(eiid, iiid, Access(direction, _, value))) =>
            Event(eiid, iiid, Access(direction, location, value))
          case _ => sys.error("Cannot verify non read/write events (such as memory barriers)")
        }
      }
    }
    poEvents
  }
}
