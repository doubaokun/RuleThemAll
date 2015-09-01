package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.Trigger
import sta.model.triggers.Implicits._

object CalendarRules extends TriggerParser[CalendarEvent] {
  import CalendarEvent._
  import white._

  def Prefix: String = Uses.categoryOf[CalendarEvent]

  override val Suffix: Option[P[Trigger.Standalone[_ <: CalendarEvent]]] = Some(
    mapParser(State.namesToValuesMap) map (v => Trigger.Condition[CalendarEvent](_.state == v))
  )

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

  def Main: P[Trigger.Standalone[_ <: CalendarEvent]] = title | description | location | availability
}
