package pipemimic.pipeline

import pipemimic.Pipeline

class RVrMMPipelineConstructor extends PipelineConstructor {
  /**
    * Create a RISC-V rMM pipeline with `coreNumber` cores
    *
    * @param coreNumber number of cores
    * @return new pipeline
    */
  override def pipelineWithCore(coreNumber: Int): Pipeline = ???
}
