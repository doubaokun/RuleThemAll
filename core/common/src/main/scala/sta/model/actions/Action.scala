package sta.model.actions

import android.content.Context

abstract class Action { this: Product =>
  def name: String = productPrefix

  def execute()(implicit ctx: Context): Unit
}
