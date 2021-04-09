package pipemimic.pipeline

class PipelineFactory {
  def createPipeline(pipeline: String): PipelineConstructor = {
    require(pipeline != null && pipeline.nonEmpty)

    if ("RISC".equals(pipeline))
      new RISCPipelineConstructor
    else if ("SCProc".equals(pipeline))
      new SCProcPipelineConstructor
    else if ("gem5".equals(pipeline))
      new Gem5PipelineConstructor
    else if ("gem5Fixed".equals(pipeline))
      new Gem5FixedPipelineConstructor
    else if ("WR".equals(pipeline))
      new RVWRPipelineConstructor
    else if ("rWR".equals(pipeline))
      new RVrWRPipelineConstructor
    else if ("rWM".equals(pipeline))
      new RVrWMPipelineConstructor
    else if ("rMM".equals(pipeline))
      new RVrMMPipelineConstructor
    else if ("nWR".equals(pipeline))
      new RVnWRPipelineConstructor
    else if ("nMM".equals(pipeline))
      new RVnMMPipelineConstructor
    else if ("A9like".equals(pipeline))
      new RVA9LikePipelineConstructor
    else
      sys.error(s"Pipeline <$pipeline> is not implemented")
  }
}
