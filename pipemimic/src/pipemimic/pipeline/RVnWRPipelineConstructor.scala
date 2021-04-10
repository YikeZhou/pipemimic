package pipemimic.pipeline

import pipemimic.Pipeline

class RVnWRPipelineConstructor extends PipelineConstructor {
  /**
    * Create a RISC-V nWR pipeline with `coreNumber` cores
    *
    * @param coreNumber number of cores
    * @return new pipeline
    */
  override def pipelineWithCore(coreNumber: Int): Pipeline = ???
}
