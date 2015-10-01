package sta.service

import android.app.PendingIntent
import android.content.{Context, Intent}
import scala.collection.concurrent.TrieMap
import sta.common.Requirement
import sta.common.SystemServices._
import sta.model.Rule
import sta.storage.RulesStorage

class TimerMap(implicit ctx: Context, storage: RulesStorage) {
  @inline private def cancelAlarm(intent: PendingIntent) = alarmManager.cancel(intent)

  @inline private def timerIntent = new Intent(ctx, classOf[STAService])

  private[this] val byRule = TrieMap.empty[String, Seq[PendingIntent]]

  private[this] val byRequirement = TrieMap.empty[Requirement, Set[String]]

  def +=(rule: Rule): this.type = {
    for (intents <- byRule.get(rule.name); intent <- intents) {
      intent.cancel()
      cancelAlarm(intent)
    }
    val timers = rule.setAlarms(timerIntent)
    byRule += (rule.name -> timers)
    for(req <- rule.requires) {
      byRequirement.get(req) match {
        case Some(ruleNames) => byRequirement += (req -> (ruleNames + rule.name))
        case None => byRequirement += (req -> Set(rule.name))
      }
    }

    this
  }

  def -=(ruleName: String): this.type = {
    for (intents <- byRule.get(ruleName); intent <- intents) {
      intent.cancel()
      cancelAlarm(intent)
    }
    for (
      rule <- storage.get(ruleName);
      req <- rule.requires;
      rules <- byRequirement.get(req)
    ) {
      byRequirement += (req -> (rules - ruleName))
    }
    byRule -= ruleName

    this
  }

  def cancelAll(): Unit = for (
    intents <- byRule.valuesIterator;
    intent <- intents
  ) {
    intent.cancel()
    cancelAlarm(intent)
  }

  def reset(requirement: Requirement): Unit = for (
    ruleNames <- byRequirement.get(requirement);
    ruleName <- ruleNames;
    rule <- storage.get(ruleName)
  ) {
    for (intents <- byRule.get(rule.name); intent <- intents) {
      cancelAlarm(intent)
    }
    byRule += (rule.name -> rule.setAlarmsFor(requirement, timerIntent))
  }
}
