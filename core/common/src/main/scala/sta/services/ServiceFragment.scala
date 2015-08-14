package sta.services

import android.content.Intent
import kj.android.logging.Logging
import sta.model.Model

abstract class ServiceFragment[M <: Model] extends Logging {
  final type PF = PartialFunction[Intent, Option[M]]

  def apply(intent: Intent): Option[M] = {
    log.info(s"Handling intent ${intent.getAction}")
    if (handle.isDefinedAt(intent)) handle(intent) else None
  }

  def handle: PF

  protected[sta] def reactOn: Set[String]
}
