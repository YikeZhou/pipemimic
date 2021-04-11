package pipemimic.pipeline

import pipemimic.Pipeline

trait PipelineConstructor {
  /**
    * Create a pipeline with `coreNumber` cores
    * @param coreNumber number of cores
    * @return new pipeline
    */
  def pipelineWithCore(coreNumber: Int): Pipeline
}
