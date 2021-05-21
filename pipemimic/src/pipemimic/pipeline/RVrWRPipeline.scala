package pipemimic.pipeline

import pipemimic._

class RVrWRPipeline(n: Int) extends {
  /** number of intra-core stages */
  override val inCoreStageNumber: Int = 6
  /** number of off-core stages (such as main memory or shared cache) */
  override val unCoreStageNumber: Int = 2
  /** number of cores (currently only support 1 or 2) */
  override val coreNumber: Int = n
} with Pipeline {
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
              /* perform at store buffer instead of mem-stage */
              stage = stageOfCore(coreIndex, 5),
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
          /* forward data from store buffer when existing data dependency */
          sem = readAfterWriteSpecialEdges(5, 3)
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

  private def inCoreStages(currentIndex: Int): List[Stage] = {
    List(
      Stage("Fetch", FIFO, NoSpecialEdges),
      Stage("Decode", FIFO, NoSpecialEdges),
      Stage("Execute", FIFO, NoSpecialEdges),
      Stage("Memory", FIFO, NoSpecialEdges),
      Stage("WriteBack", FIFO, NoSpecialEdges),
      Stage("StoreBuffer", FIFO, storeBufferSpecialEdges(
        /* FIFO: until last write commit can write enter mem */
        srcPerformStage = stageOfCore(currentIndex, 7),
        dstPerformStage = stageOfCore(currentIndex, 6))))
  }

  private def unCoreStages: List[Stage] = {
    List(
      Stage("MainMemory", NoOrderGuarantees, NoSpecialEdges),
      Stage("Retire", FIFO, NoSpecialEdges))
  }
}
