package rta.parser.triggers

import fastparse.noApi._
import scala.concurrent.duration._
import rta.common.Uses
import rta.model.triggers.Implicits._
import rta.model.triggers.Trigger
import rta.parser.TriggerParser

object CalendarRules extends TriggerParser[CalendarEvent] {
  import CalendarEvent._
  import white._

  def Prefix: String = Uses.categoryOf[CalendarEvent]

  private def title: P[Trigger.Condition[CalendarEvent]] = {
    "title".withWS ~ matchStringParser(_.title)
  }

  private def description: P[Trigger.Condition[CalendarEvent]] = {
    "description".withWS ~ matchStringParser(_.description)
  }

  private def location: P[Trigger.Condition[CalendarEvent]] = {
    "location".withWS ~ matchStringParser(_.location)
  }

  private def availability: P[Trigger.Condition[CalendarEvent]] = {
    "availability".withWS ~ "is".withWS ~ mapParser(Availability.namesToValuesMap) map (v =>
      Trigger.Condition[CalendarEvent](_.availability == v)
    )
  }

  def Main: P[Trigger.Condition[_ <: CalendarEvent]] = title | description | location | availability

  override def Rule: P[Trigger.Standalone[_ <: CalendarEvent]] = {
    (Main.map(Seq(_)) | ("(" ~ Main.rep(min = 1, sep = ",") ~ ")").?.map(_.getOrElse(Seq.empty))) ~
      mapParser(State.namesToValuesMap) map { case (conditions, state) =>
      Trigger.Timer.dynamic(24.hours, Uses.materializeUses[CalendarEvent].requirements) {
        case (from, window, ctx) => state.find(from, window, ctx, conditions: _*)
      }
    }
  }
}
