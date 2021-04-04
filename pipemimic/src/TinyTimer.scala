package pipemimic

class TinyTimer(name: String) {
  var start: Long = _
  var init = false

  def reset(): Unit = {
    init = true
    start = System.nanoTime()
  }

  override def toString: String = {
    val timeElapsed = (System.nanoTime() - start) / 1000000
    if (init) s"Timer<$name>: $timeElapsed ms" else "Error: not initialized"
  }
}
