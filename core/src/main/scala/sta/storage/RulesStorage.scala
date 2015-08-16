package sta.storage

import android.content.Context
import java.io.File
import kj.android.common.AppInfo
import kj.android.logging.Logging
import sta.model.Rule
import sta.model.triggers.EmptyTrigger

abstract class RulesStorage extends Logging {
  def ctx: Context

  def info: AppInfo

  def unregister(names: String*): Set[String]

  def register(from: File): (Set[String], Set[String])

  def allRules: Iterator[Rule]

  def rules: Iterator[Rule] = allRules.filterNot(_.trigger == EmptyTrigger)

  def startupRules: Iterator[Rule] = allRules.filter(_.trigger == EmptyTrigger)
}
