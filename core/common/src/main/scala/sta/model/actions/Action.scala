package sta.model.actions

import android.content.Context

/** Base for all executable actions. */
abstract class Action { this: Product =>
  def name: String = productPrefix

  def execute()(implicit ctx: Context): Unit
}
