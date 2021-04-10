package pipemimic.pipeline

import pipemimic.Pipeline

class RVnMMPipelineConstructor extends PipelineConstructor {
  /**
    * Create a RISC-V nMM pipeline with `coreNumber` cores
    *
    * @param coreNumber number of cores
    * @return new pipeline
    */
  override def pipelineWithCore(coreNumber: Int): Pipeline = ???
}
