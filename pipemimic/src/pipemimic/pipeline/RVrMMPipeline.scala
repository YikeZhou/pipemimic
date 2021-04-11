package pipemimic.pipeline

import pipemimic._

class RVrMMPipeline(n: Int) extends Pipeline {
  /** described basic features of this pipeline */
  override val pipeName: String = "RISC-V rMM"
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
          path = stageOfCore(coreIndex, List.range(0, 5)) ::: stageOfCore(coreIndex, List.range(6, 9)),
          performStages = List(
            PerformStages(
              stage = stageOfCore(coreIndex, 4 /* execute stage */),
              cores = List.range(0, n),
              observability = List.range(0, n),
              cacheLineInvLoc = Some(stageOfCore(coreIndex, 5 /* cache invalidate */)),
              isMainMemory = true
            )
          ),
          sem = loadSpecialEdges(
            loadPerformStage = stageOfCore(coreIndex, 4),
            cacheInvPerformStage = stageOfCore(coreIndex, 5)
          )
        ))
      case Some(Direction.W) => List(
        PathOption(
          optionName = s"Write${e.addr.get}",
          evt = e,
          path = stageOfCore(coreIndex, List.range(0, 5) ::: stageOfCore(coreIndex, List.range(6, 11))),
          performStages = List(
            /* write to store buffer */
            PerformStages(
              stage = stageOfCore(coreIndex, 2 /* rename */),
              cores = List(coreIndex),
              observability = List(coreIndex),
              cacheLineInvLoc = None,
              isMainMemory = false
            ),
            /* write to main memory */
            PerformStages(
              stage = stageOfCore(coreIndex, 9),
              cores = List.range(0, n),
              observability = List.range(0, n),
              cacheLineInvLoc = None,
              isMainMemory = true
            )
          ),
          sem = storeLoadSpecialEdges(
            storePerformStage = stageOfCore(coreIndex, 2),
            loadPerformStage = stageOfCore(coreIndex, 4)
          )
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
      Stage("Rename", FIFO, NoSpecialEdges),
      Stage("Issue", NoOrderGuarantees, NoSpecialEdges),
      Stage("Execute", NoOrderGuarantees, NoSpecialEdges),
      Stage("CacheLineInvalidate", NoOrderGuarantees, NoSpecialEdges),
      Stage("WriteBack", NoOrderGuarantees, NoSpecialEdges),
      Stage("Commit", restore(stageOfCore(currentIndex, 2)), NoSpecialEdges),
      Stage("StoreBuffer", sameAddressOrdered, storeBufferSpecialEdges(
        srcPerformStage = stageOfCore(currentIndex, 10),
        dstPerformStage = stageOfCore(currentIndex, 8)
      )))
  }

  /** number of off-core stages (such as main memory or shared cache) */
  override val unCoreStageNumber: Int = unCoreStages.length

  private def unCoreStages: List[Stage] = {
    List(
      Stage("L2CacheForWrites", NoOrderGuarantees, NoSpecialEdges),
      Stage("Retire", NoOrderGuarantees, NoSpecialEdges))
  }
}
