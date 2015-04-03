package sta.parser.triggers

import org.parboiled2._
import sta.model.Model
import sta.model.triggers.{ AtomicTrigger, Trigger }
import sta.parser.{ BasicRules, RichParser }

import scala.language.implicitConversions

trait TriggerParser extends RichParser with BasicRules {
  def MainT: Rule1[Trigger]
}

trait TriggerParserPart { this: TriggerParser ⇒
  protected def MainT: Rule1[Trigger]

  protected def prefix: String
}
