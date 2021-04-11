package pipemimic.pipeline

import pipemimic.Pipeline

class RVrWMPipelineConstructor extends PipelineConstructor
{
  /**
    * Create a RISC-V rWM pipeline with `coreNumber` cores
    *
    * @param coreNumber number of cores
    * @return new pipeline
    */
  override def pipelineWithCore(coreNumber: Int): Pipeline = new RVrWMPipeline(coreNumber)
}
