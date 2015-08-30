package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.Condition
import sta.model.triggers.Implicits._

object CalendarRules extends TriggerParser[CalendarEvent] {
  import CalendarEvent._
  import white._

  def Prefix: String = Uses.categoryOf[CalendarEvent]

  override val Suffix: Option[P[Condition.Trigger[_ <: CalendarEvent]]] = Some(
    mapParser(State.namesToValuesMap) map (v => Condition.Trigger[CalendarEvent](_.state == v))
  )

  private def title: P[Condition.Trigger[CalendarEvent]] = {
    "title" ~ matchStringParser[CalendarEvent](_.title)
  }

  private def description: P[Condition.Trigger[CalendarEvent]] = {
    "description" ~ matchStringParser[CalendarEvent](_.description)
  }

  private def location: P[Condition.Trigger[CalendarEvent]] = {
    "location" ~ matchStringParser[CalendarEvent](_.location)
  }

  private def availability: P[Condition.Trigger[CalendarEvent]] = {
    "availability" ~ "is" ~ mapParser(Availability.namesToValuesMap) map (v =>
      Condition.Trigger[CalendarEvent](_.availability == v)
    )
  }

  val Rule: P[Condition.Standalone[_ <: CalendarEvent]] = title | description | location | availability
}
