package sta.storage

import android.content.Context
import java.io.File
import kj.android.common.AppInfo
import kj.android.logging.Logging
import sta.model.Rule

abstract class RulesStorage extends Logging {
  def ctx: Context

  def info: AppInfo

  /** Unregisters rules.
    *
    * @param names rules to unregister
    * @return set of removed intent hash codes
    */
  def unregister(names: String*): Set[Int]

  /** Registers rules.
    *
    * @param from location of rule files
    * @return tuple (set of added intent hash codes, set of removed intent hash codes, set of added rule names)
    */
  def register(from: File*): RegistrationInfo

  def allRules: Iterator[Rule]

  def rules: Iterator[Rule] = allRules.filter(_.branches.nonEmpty)

  def get(name: String): Option[Rule]

  def startupRules: Iterator[Rule] = allRules.filter(_.branches.isEmpty)
}
