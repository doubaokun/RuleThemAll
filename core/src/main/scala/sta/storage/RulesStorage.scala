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

  /** Unregisters rules.
   *
   *  @param names rules to unregister
   *  @return set of removed intent hash codes
   */
  def unregister(names: String*): Set[Int]

  /** Register rules
   *
   *  @param from path to rules
   *  @return tuple (set of added intent hash codes, set of removed intent hash codes)
   */
  def register(from: File): (Set[Int], Set[Int])

  def allRules: Iterator[Rule]

  def rules: Iterator[Rule] = allRules.filterNot(_.trigger == EmptyTrigger)

  def startupRules: Iterator[Rule] = allRules.filter(_.trigger == EmptyTrigger)
}
