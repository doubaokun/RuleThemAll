package sta.services

import android.content.Context

trait RulesExecutor extends Context {
  def resetTimers(): Unit // reset timers selectively
}
