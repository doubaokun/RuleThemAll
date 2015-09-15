package sta.model.actions

import android.content.Context

/** Base for all executable actions. */
abstract class Action extends Serializable { this: Product =>
  def name: String = productPrefix

  def execute()(implicit ctx: Context): Unit
}
