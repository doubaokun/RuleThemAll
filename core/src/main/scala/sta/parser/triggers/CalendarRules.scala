package sta.parser.triggers

import fastparse.noApi._
import scala.concurrent.duration._
import sta.common.Uses
import sta.model.triggers.Implicits._
import sta.model.triggers.Trigger

object CalendarRules extends TriggerParser[CalendarEvent] {
  import CalendarEvent._
  import white._

  def Prefix: String = Uses.categoryOf[CalendarEvent]

  private def title: P[Trigger.Condition[CalendarEvent]] = {
    "title" ~ matchStringParser[CalendarEvent](_.title)
  }

  private def description: P[Trigger.Condition[CalendarEvent]] = {
    "description" ~ matchStringParser[CalendarEvent](_.description)
  }

  private def location: P[Trigger.Condition[CalendarEvent]] = {
    "location" ~ matchStringParser[CalendarEvent](_.location)
  }

  private def availability: P[Trigger.Condition[CalendarEvent]] = {
    "availability" ~ "is" ~ mapParser(Availability.namesToValuesMap) map (v =>
      Trigger.Condition[CalendarEvent](_.availability == v)
    )
  }

  def Main: P[Trigger.Condition[_ <: CalendarEvent]] = title | description | location | availability

  override lazy val Rule: P[Trigger] = {
    (Main.map(Seq(_)) | ("(" ~ Main.rep(min = 1, sep = ",") ~ ")").?.map(_.getOrElse(Seq.empty))) ~
      mapParser(State.namesToValuesMap) map { case (conditions, state) =>
      Trigger.Timer.dynamic(24.hours, Uses.materializeUses[CalendarEvent].requirements) {
        case (from, window, ctx) => state.find(from, window, ctx, conditions: _*)
      }
    }
  }
}
