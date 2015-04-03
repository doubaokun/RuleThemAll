package sta.services

import android.content.Intent
import sta.model.Model

abstract class ServiceFragment[+M <: Model] {
  def handle(intent: Intent): List[M]

  protected[sta] def reactOn: Set[String]
}
