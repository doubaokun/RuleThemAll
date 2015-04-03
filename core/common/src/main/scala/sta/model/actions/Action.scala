package sta.model.actions

import android.content.Context
import kj.android.common.UsedFeatures

abstract class Action { this: Product ⇒
  def name: String = productPrefix

  def execute()(implicit ctx: Context): Unit
}
