package pipemimic.pipeline
import pipemimic.Stages

class RVrWRPipelineConstructor extends PipelineConstructor {
  /**
    * Create a RISC-V rWR pipeline with `coreNumber` cores
    *
    * @param coreNumber number of cores
    * @return new pipeline
    */
  override def pipelineWithCore(coreNumber: Int): Stages.Pipeline = ???
}
