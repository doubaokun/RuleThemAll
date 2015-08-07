package sta.storage

import android.content.Context
import java.io.File
import kj.android.logging.Logging
import sta.model.Rule

abstract class RulesStorage extends Logging {
  def ctx: Context

  def unregister(names: String*): Set[String]

  def register(from: File): Set[String]

  def rules: Iterator[Rule]
}
