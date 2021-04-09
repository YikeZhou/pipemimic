package pipemimic

import pipemimic.Stages.{GlobalEvent, PathOption, Scenario}

import scala.collection.mutable.ListBuffer

package object execution {

  object LitmusTestExpectedResult extends Enumeration {
    val Permitted, Forbidden = Value
  }

  implicit class ReadsFromPrinter(rf: (Option[Event], Event)) {
    override def toString: String = rf match {
      case (Some(es), ed) => s"${es.eiid}-rf->${ed.eiid}"
      case (None, ed) => s"initValue-rf->${ed.eiid}"
    }
  }

  def candidateName(expected: LitmusTestExpectedResult.Value, rf: List[(Eiid, Eiid)]): String = {
    s"(exp: ${ if (expected == LitmusTestExpectedResult.Forbidden) "Forbidden" else "Permitted" }) RF: " +
      rf.map { case (w, r) => s"($w-rf->$r)" }.mkString
  }

  def executionEdgeLabel(edgeName: String, edges: List[(GlobalEvent, GlobalEvent, String)]): String = {
    val names = edges map { edge =>
      val (src, dst) = (edge._1._2, edge._2._2) /* get eiid */
      s"($src-$edgeName->$dst)"
    }
    names.mkString(" ")
  }

  implicit class GetEventPath(s: Scenario) {
    def pathOfEvent(eiid: Eiid): Option[PathOption] = {
      for (pathOption <- s if pathOption.evt.eiid == eiid) return Some(pathOption)
      None
    }
  }

  def performStagesWithRespectToCore(core: Int, po: PathOption): List[Int] = {
    val performStages = po.performStages
    val result = ListBuffer.empty[Int]
    for (performStage <- performStages if performStage.cores.contains(core)) result += performStage.stage
    result.toList
  }

  def visibleStagesWithRespectToCore(core: Int, po: PathOption): List[Int] = {
    val result = ListBuffer.empty[Int]
    for (performStage <- po.performStages if performStage.observability.contains(core)) result += performStage.stage
    result.toList
  }

  def performOrInvalidStagesWithRespectToCore(core: Int, po: PathOption): List[Int] = {
    val result = ListBuffer.empty[Int]
    for (performStage <- po.performStages if performStage.cores.contains(core)) {
      if (performStage.cacheLineInvLoc.isDefined)
        result += performStage.cacheLineInvLoc.get
      else
        result += performStage.stage
    }
    result.toList
  }
}
