package pipemimic

package object execution {

  implicit class ReadsFromPrinter(rf: (Option[Event], Event)) {
    override def toString: String = rf match {
      case (Some(es), ed) => s"${es.eiid}-rf->${ed.eiid}"
      case (None, ed) => s"initValue-rf->${ed.eiid}"
    }
  }

  def candidateName(expected: LitmusTestResult.Value, rf: List[(Eiid, Eiid)]): String = {
    s"(exp: ${ if (expected == LitmusTestResult.Forbidden) "Forbidden" else "Permitted" }) RF: " +
      rf.map { case (w, r) => s"($w-rf->$r)" }.mkString
  }
}
