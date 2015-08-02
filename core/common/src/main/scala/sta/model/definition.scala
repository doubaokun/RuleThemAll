package sta.model

import scala.language.implicitConversions

import android.content.Context
import scala.collection.immutable
import scala.util.control.NonFatal
import scalaz.Scalaz._
import scalaz._
import shapeless.HMap
import sta.model.actions.Action
import sta.model.triggers.Trigger

trait DefName

// TODO: separator that can't be read as UTF8 string ??
// | name | sep | trigger | sep | action | list_sep | action ... |
// | name_length | name | ??? | sep | action | list_sep | action
case class Definition(name: String @@ DefName, trigger: Trigger, actions: Seq[Action]) {
  @inline private implicit def executeAction(a: Action): ValidationNel[(String, Throwable), Unit] = try {
    Success(())
  } catch {
    case NonFatal(t) => Failure(NonEmptyList((a.name, t)))
  }

  def execute(state: HMap[ModelKV])(implicit ctx: Context): \/[Definition, ValidationNel[(String, Throwable), Unit]] = {
    if (trigger.satisfiedBy(state)) {
      val f = ((_: Unit, _: Unit) => ()).successNel[(String, Throwable)]
      \/-(
        actions.foldLeft(().successNel[(String, Throwable)]) {
          case (acc, action) =>
            Apply[({ type l[a] = ValidationNel[(String, Throwable), a] })#l].ap2(acc, action)(f)
        }
      )
    } else -\/(this) // TODO partial evaluate
  }

  def uses: Set[String] = trigger.uses

  override def hashCode(): Int = Tag.unwrap(name).hashCode
}

