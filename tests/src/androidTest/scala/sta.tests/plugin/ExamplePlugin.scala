package rta.tests.plugin

import android.content._
import fastparse.noApi._
import rta.common.Toast
import rta.logging.Logging
import rta.model.actions.Action
import rta.model.triggers.Trigger
import rta.model.triggers.Trigger.Standalone
import rta.model.{Model, ModelCompanion}
import rta.parser.{ActionParser, TriggerParser}
import rta.plugin.Plugin
import rta.tests.plugin.ExamplePlugin._

class ExamplePlugin extends Plugin[ShowToast, Dummy] {
  override def actionParser: Option[ActionParser[ShowToast]] = Some(ToastActionParser)

  override def triggerParser: Option[TriggerParser[Dummy]] = Some(DummyTriggerParser)
}

object ExamplePlugin {
  case class Dummy() extends Model[Dummy](Dummy)
  implicit object Dummy extends ModelCompanion[Dummy]

  case class ShowToast(txt: String) extends Action with Logging {
    def execute()(implicit ctx: Context): Unit = Toast(txt, Toast.Long)
  }

  object ToastActionParser extends ActionParser[ShowToast] {
    import white._

    def Rule: P[ShowToast] = "show" ~ String map ShowToast
  }
  
  object DummyTriggerParser extends TriggerParser[Dummy] {

    def Prefix: String = "dummy"

    def Main: P[Standalone[_ <: Dummy]] = P("available" push Trigger.Condition[Dummy](_ == Dummy()))
  }
}
