package sta.model.actions

import android.content.Context

/** Base for all executable actions. */
abstract class Action extends Serializable { this: Product =>
  def name: String = productPrefix

  def execute()(implicit ctx: Context): Unit

  def prepare()(implicit ctx: Context): Unit = {}
}
