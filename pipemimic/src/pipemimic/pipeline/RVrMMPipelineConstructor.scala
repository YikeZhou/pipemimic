package pipemimic.pipeline
import pipemimic.Stages

class RVrMMPipelineConstructor extends PipelineConstructor {
  /**
    * Create a RISC-V rMM pipeline with `coreNumber` cores
    *
    * @param coreNumber number of cores
    * @return new pipeline
    */
  override def pipelineWithCore(coreNumber: Int): Stages.Pipeline = ???
}
