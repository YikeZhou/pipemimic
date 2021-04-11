package pipemimic.pipeline

import pipemimic.Pipeline

class RVWRPipelineConstructor extends PipelineConstructor {
  /**
    * Create a RISC-V WR pipeline with `coreNumber` cores
    *
    * @param coreNumber number of cores
    * @return new pipeline
    */
  override def pipelineWithCore(coreNumber: Int): Pipeline = new RVWRPipeline(coreNumber)
}
