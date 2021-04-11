package pipemimic.pipeline

class PipelineFactory {
  def createPipeline(pipeline: String): PipelineConstructor = {
    require(pipeline != null && pipeline.nonEmpty)

    if ("WR".equals(pipeline))
      new RVWRPipelineConstructor
    else if ("rWR".equals(pipeline))
      new RVrWRPipelineConstructor
    else if ("rWM".equals(pipeline))
      new RVrWMPipelineConstructor
    else if ("rMM".equals(pipeline))
      new RVrMMPipelineConstructor
    else
      sys.error(s"Pipeline <$pipeline> is not implemented")
  }
}
