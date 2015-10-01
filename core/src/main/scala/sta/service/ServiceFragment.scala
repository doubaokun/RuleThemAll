package sta.service

import android.content.{Context, Intent}
import kj.android.logging.Logging
import sta.model.BaseModel

/** Service fragment that is responsible for updating models of a given type.
  *
  * Note that it should have either no-arg constructor or
  * constructor with single [[RulesExecutor]] argument.
  */
abstract class ServiceFragment[M <: BaseModel] extends Logging {
  final type PF = PartialFunction[Intent, Option[M]]

  def apply(intent: Intent): Option[M] = {
    log.info(s"Handling intent ${intent.getAction}")
    if (handle.isDefinedAt(intent)) handle(intent) else None
  }

  def handle: PF
}
