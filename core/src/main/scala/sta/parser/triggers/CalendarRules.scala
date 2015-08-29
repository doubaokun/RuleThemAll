package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.Trigger
import sta.model.triggers.Implicits._

object CalendarRules extends TriggerParser[CalendarEvent] {
  import CalendarEvent._
  import white._

  def Prefix: String = Uses.categoryOf[CalendarEvent]

  override val Suffix: Option[P[Trigger.Atomic[_ <: CalendarEvent]]] = Some(
    mapParser(State.namesToValuesMap) map (v => Trigger.Atomic[CalendarEvent](_.state == v))
  )

  private def title: P[Trigger.Atomic[CalendarEvent]] = {
    "title" ~ matchStringParser[CalendarEvent](_.title)
  }

  private def description: P[Trigger.Atomic[CalendarEvent]] = {
    "description" ~ matchStringParser[CalendarEvent](_.description)
  }

  private def location: P[Trigger.Atomic[CalendarEvent]] = {
    "location" ~ matchStringParser[CalendarEvent](_.location)
  }

  private def availability: P[Trigger.Atomic[CalendarEvent]] = {
    "availability" ~ "is" ~ mapParser(Availability.namesToValuesMap) map (v =>
      Trigger.Atomic[CalendarEvent](_.availability == v)
    )
  }

  val Rule: P[Trigger.Atomic[_ <: CalendarEvent]] = title | description | location | availability
}
