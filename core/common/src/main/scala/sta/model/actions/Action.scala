package sta.model.actions

import android.content.Context

abstract class Action extends Serializable { this: Product =>
  def name: String = productPrefix

  def execute()(implicit ctx: Context): Unit
}
