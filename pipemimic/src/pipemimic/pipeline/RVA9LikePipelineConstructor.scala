package pipemimic.pipeline
import pipemimic.Stages

class RVA9LikePipelineConstructor extends PipelineConstructor {
  /**
    * Create a RISC-V A9 like pipeline with `coreNumber` cores
    *
    * @param coreNumber number of cores
    * @return new pipeline
    */
  override def pipelineWithCore(coreNumber: Int): Stages.Pipeline = ???
}