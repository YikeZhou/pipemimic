package pipemimic.pipeline

import pipemimic.Pipeline

class RVrWRPipelineConstructor extends PipelineConstructor {
  /**
    * Create a RISC-V rWR pipeline with `coreNumber` cores
    *
    * @param coreNumber number of cores
    * @return new pipeline
    */
  override def pipelineWithCore(coreNumber: Int): Pipeline = new RVrWRPipeline(coreNumber)
}
