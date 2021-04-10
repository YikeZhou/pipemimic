package pipemimic.pipeline

import pipemimic.Pipeline

class Gem5PipelineConstructor extends PipelineConstructor {
  /**
    * Create a gem5 O3 pipeline with `coreNumber` cores
    *
    * @param coreNumber number of cores
    * @return new pipeline
    */
  override def pipelineWithCore(coreNumber: Int): Pipeline = ???
}
