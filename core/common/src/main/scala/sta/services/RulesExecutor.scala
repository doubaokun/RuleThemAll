package sta.services

import android.content.Context
import sta.common.Requirement

trait RulesExecutor extends Context {
  def resetTimers(requirements: Set[Requirement]): Unit
}
