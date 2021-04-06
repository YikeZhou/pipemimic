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

  type Location = Address /* a memory location is specified by an address */

  abstract class Action /* an access specified by polarity + location + value */
  case class Access(d: Direction.Value, l: Location, v: Value) extends Action
  case class MemoryFence() extends Action

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

    def loc: Location = {
      this.action match {
        case Access(_, l, _) => l
      }
    }

    def isWrite: Boolean = this.dirn == Direction.W
  }
}