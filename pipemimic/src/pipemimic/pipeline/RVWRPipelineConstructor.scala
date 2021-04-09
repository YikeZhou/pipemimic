package pipemimic.pipeline
import pipemimic.Stages

class RVWRPipelineConstructor extends PipelineConstructor {
  /**
    * Create a RISC-V WR pipeline with `coreNumber` cores
    *
    * @param coreNumber number of cores
    * @return new pipeline
    */
  override def pipelineWithCore(coreNumber: Int): Stages.Pipeline = ???
}
