package pipemimic.statistics

class TinyTimer(name: String) {
  var start: Long = _
  var init = false
  var timeElapsed: Long = 0

  def reset(): Unit = {
    init = true
    start = System.nanoTime()
  }

  override def toString: String = {
    timeElapsed = (System.nanoTime() - start) / 1000000
    if (init) s"Timer<$name>: $timeElapsed ms" else s"Error: Timer<$name> not initialized"
  }
}

object TinyTimer {
  def apply(name: String): TinyTimer = {
    val timer = new TinyTimer(name)
    timer.reset()
    timer
  }
}