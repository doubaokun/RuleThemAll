package sta.services

import android.content.Context

trait RuleExecutor extends Context {
  def resetTimers(): Unit // reset timers selectively
}
