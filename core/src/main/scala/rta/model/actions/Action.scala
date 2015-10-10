package rta.model.actions

import android.content.Context

/** Base for all executable actions. */
@SuppressWarnings(Array("org.brianmckenna.wartremover.warts.ToString"))
abstract class Action extends Serializable {
  def name: String = toString

  def kind: ActionKind = this.getClass -> None

  def execute()(implicit ctx: Context): Unit

  def prepare()(implicit ctx: Context): Unit = {}
}
