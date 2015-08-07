package sta.model

import scala.language.implicitConversions

import android.content.Context
import scala.util.control.NonFatal
import scalaz.Scalaz._
import scalaz._
import shapeless.HMap
import sta.model.actions.Action
import sta.model.triggers.Trigger

case class Rule(name: String, trigger: Trigger, actions: Seq[Action]) extends Serializable {
  @inline private implicit def executeAction(a: Action): ValidationNel[(String, Throwable), Unit] = try {
    Success(())
  } catch {
    case NonFatal(t) => Failure(NonEmptyList((a.name, t)))
  }

  // TODO rete
  def execute(state: HMap[ModelKV])(implicit ctx: Context): \/[Rule, ValidationNel[(String, Throwable), Unit]] = {
    if (trigger.satisfiedBy(state)) {
      val f = ((_: Unit, _: Unit) => ()).successNel[(String, Throwable)]
      \/-(
        actions.foldLeft(().successNel[(String, Throwable)]) {
          case (acc, action) =>
            Apply[({ type l[a] = ValidationNel[(String, Throwable), a] })#l].ap2(acc, action)(f)
        }
      )
    } else -\/(this)
  }

  def uses: Set[String] = trigger.uses

  override def hashCode(): Int = name.hashCode
}

