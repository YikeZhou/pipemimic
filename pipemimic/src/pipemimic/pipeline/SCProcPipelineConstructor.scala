package pipemimic.pipeline
import pipemimic.Stages

class SCProcPipelineConstructor extends PipelineConstructor {
  /**
    * Create a Sequential Consistent Processor pipeline with `coreNumber` cores
    *
    * @param coreNumber number of cores
    * @return new pipeline
    */
  override def pipelineWithCore(coreNumber: Int): Stages.Pipeline = ???
}
