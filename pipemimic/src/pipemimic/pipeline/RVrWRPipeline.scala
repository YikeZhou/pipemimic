package pipemimic.pipeline

import pipemimic._

class RVrWRPipeline(n: Int) extends Pipeline {
  /** described basic features of this pipeline */
  override val pipeName: String = "RISC-V rWR"
  /** all stages of this pipeline */
  override val stages: List[Stage] =
    List.tabulate(n)(inCoreStages).flatten ::: unCoreStages
  /** map given event into list of its possible path options */
  override val pathsFor: Event => PathOptions = { e =>
    val coreIndex = e.iiid.proc
    e.dirn match {
      case Some(Direction.R) => List(
        /* read from main memory */
        PathOption(
          optionName = s"Read${e.addr.get}",
          evt = e,
          path = stageOfCore(coreIndex, List.range(0, 5)),
          performStages = List(
            PerformStages(
              stage = stageOfCore(coreIndex, 3),
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
          path = stageOfCore(coreIndex, List.range(0, 5)),
          performStages = List(
            PerformStages(
              stage = stageOfCore(coreIndex, 3),
              cores = List.range(0, n),
              observability = List(coreIndex),
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
          path = stageOfCore(coreIndex, List.range(0, 8)),
          performStages = List(
            /* write to store buffer */
            PerformStages(
              stage = stageOfCore(coreIndex, 3),
              cores = List(coreIndex),
              observability = List(coreIndex),
              cacheLineInvLoc = None,
              isMainMemory = false
            ),
            /* write to main memory */
            PerformStages(
              stage = stageOfCore(coreIndex, 6),
              cores = List.range(0, n),
              observability = List.range(0, n),
              cacheLineInvLoc = None,
              isMainMemory = true
            )
          ),
          sem = NoSpecialEdges
        ))
      case _ /* Memory Fence */ => List(
        PathOption(
          optionName = "Fence",
          evt = e,
          path = stageOfCore(coreIndex, List.range(0, 5)),
          performStages = Nil,
          sem = fenceTSOSpecialEdges(
            storePerformStage = stageOfCore(coreIndex, 6),
            loadPerformStage = stageOfCore(coreIndex, 3)
          )
        )
      )
    }
  }

  /** number of cores (currently only support 1 or 2) */
  override val coreNumber: Int = n
  /** number of intra-core stages */
  override val inCoreStageNumber: Int = inCoreStages(0).length

  private def inCoreStages(currentIndex: Int): List[Stage] = {
    List(
      Stage("Fetch", FIFO, NoSpecialEdges),
      Stage("Decode", FIFO, NoSpecialEdges),
      Stage("Execute", FIFO, NoSpecialEdges),
      Stage("Memory", FIFO, NoSpecialEdges),
      Stage("WriteBack", FIFO, NoSpecialEdges),
      Stage("StoreBuffer", FIFO, storeBufferSpecialEdges(
        srcPerformStage = stageOfCore(currentIndex, 7),
        dstPerformStage = stageOfCore(currentIndex, 5))))
  }

  /** number of off-core stages (such as main memory or shared cache) */
  override val unCoreStageNumber: Int = unCoreStages.length

  private def unCoreStages: List[Stage] = {
    List(
      Stage("MainMemory", NoOrderGuarantees, NoSpecialEdges),
      Stage("Retire", FIFO, NoSpecialEdges))
  }
}
