package pipemimic

import pipemimic.Stages.{LocalReordering, SpecialEdgeMap}

package object pipeline {
  // TODO add useful definitions such as FIFO, NoSpecialEdges, etc.

  /* Local Reordering */

  /**
    * any ordering guaranteed at the input is also guaranteed at the output. This is the common case.
    */
  val FIFO: LocalReordering = _ => ordering => ordering

  /**
    * Operations can leave the stage in any order; nothing is guaranteed.
    */
  val NoOrderGuarantees: LocalReordering = _ => _ => Nil

  /**
    * The output order is guaranteed to match some previous ordering
    */
  def Restore(n: Int): LocalReordering = e => _ => e.nthDefault(n, Nil)

  /* Special Edge Maps */

  /**
    * In most cases, we don't need to add any special edges
    */
  val NoSpecialEdges: SpecialEdgeMap = _ => _ => _ => Nil

}
