package pipemimic.pipeline
import pipemimic.Stages

class RISCPipelineConstructor extends PipelineConstructor {
  /**
    * Create a 5-stage RISC pipeline with `coreNumber` cores
    *
    * @param coreNumber number of cores
    * @return new pipeline
    */
  override def pipelineWithCore(coreNumber: Int): Stages.Pipeline = ???
}
