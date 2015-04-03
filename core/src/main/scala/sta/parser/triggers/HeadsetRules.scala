package sta.parser.triggers

import kj.android.common.UsedFeatures
import org.parboiled2.Rule1
import sta.model.system._
import sta.model.triggers.functions.EqualFunction
import sta.model.triggers.{ AtomicTrigger, Trigger }
import kj.android.common.UsedFeatures._

import scala.language.implicitConversions
import scalaz._

trait HeadsetRules extends TriggerParserPart { this: TriggerParser ⇒
  protected def prefix: String = implicitly[UsedFeatures[Headset]].category

  private implicit def eq: Equal[Headset] = Equal.equalA

  private def connectivity: Rule1[AtomicTrigger[Headset]] = rule(
    (endWS("connected") ~ push(AtomicTrigger[Headset](EqualFunction(Headset.Connected)))) |
      (endWS("disconnected") ~ push(AtomicTrigger[Headset](EqualFunction(Headset.Disconnected))))
  )

  protected def MainT: Rule1[Trigger] = connectivity
}
