package pipemimic.pipeline

import pipemimic._

class RISCPipeline(n: Int) extends Pipeline {
  /** described basic features of this pipeline */
  override val pipeName: String = "RISC"
  /** all stages of this pipeline */
  override val stages: List[Stage] = List.tabulate(n)(RISCPipelineStages(n, _)).flatten ::: RISCSharedStages
  /** map given event into list of its possible path options */
  override val pathsFor: Event => PathOptions = { e =>
    e.dirn match {
      case Some(Direction.R) => List(
        /* read from main memory */
        PathOption(
          optionName = s"Read${e.addr.get}",
          evt = e,
          path = stageOfCore(e.iiid.proc, 0 until 5),
          performStages = List(
            PerformStages(
              stage = stageOfCore(e.iiid.proc, 3),
              cores = List.range(0, n),
              observability = List.range(0, n),
              cacheLineInvLoc = None,
              isMainMemory = true
            )
          ),
          sem = NoSpecialEdges
        ),
        /* read from store buffer */
        PathOption(
          optionName = s"STBFwd${e.addr.get}",
          evt = e,
          path = stageOfCore(e.iiid.proc, List.range(0, 5)),
          performStages = List(
            PerformStages(
              stage = stageOfCore(e.iiid.proc, 3),
              cores = List.range(0, n),
              observability = List(e.iiid.proc),
              cacheLineInvLoc = None,
              isMainMemory = false
            )
          ),
          sem = NoSpecialEdges
        )
      )
      case Some(Direction.W) => List(
        PathOption(
          optionName = s"Write${e.addr.get}",
          evt = e,
          path = stageOfCore(e.iiid.proc, List.range(0, 6)) ::: stageOfCore(n, List.range(0, 2)),
          performStages = List(
            /* write to store buffer */
            PerformStages(
              stage = stageOfCore(e.iiid.proc, 3),
              cores = List(e.iiid.proc),
              observability = List(e.iiid.proc),
              cacheLineInvLoc = None,
              isMainMemory = false
            ),
            /* write to main memory */
            PerformStages(
              stage = stageOfCore(e.iiid.proc, 0),
              cores = List.range(0, n),
              observability = List.range(0, n),
              cacheLineInvLoc = None,
              isMainMemory = true
            )
          ),
          sem = NoSpecialEdges
        )
      )
      case _ => List(
        PathOption(
          optionName = "Fence",
          evt = e,
          path = stageOfCore(e.iiid.proc, List.range(0, 5)),
          performStages = Nil,
          sem = FenceTSOSpecialEdges(e.iiid.proc, n)
        )
      )
    }
  }
  /** number of cores (currently only support 1 or 2) */
  override val coreNumber: Location = n
  /** number of intra-core stages */
  override val inCoreStageNumber: Location = 6

  private def RISCPipelineStages(n: Int, c: Int): List[Stage] = {
    List(
      Stage("Fetch", FIFO, NoSpecialEdges),
      Stage("Decode", FIFO, NoSpecialEdges),
      Stage("Execute", FIFO, NoSpecialEdges),
      Stage("Memory", FIFO, NoSpecialEdges),
      Stage("WriteBack", FIFO, NoSpecialEdges),
      Stage("StoreBuffer", FIFO, StoreBufferSpecialEdges(c, n)),
    )
  }

  /** number of off-core stages (such as main memory or shared cache) */
  override val unCoreStageNumber: Location = 2

  private def RISCSharedStages: List[Stage] = {
    List(
      Stage("MainMemory", NoOrderGuarantees, NoSpecialEdges),
      Stage("Retire", FIFO, NoSpecialEdges)
    )
  }
}
