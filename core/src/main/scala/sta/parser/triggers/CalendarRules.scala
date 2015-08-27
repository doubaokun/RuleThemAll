package sta.parser.triggers

import fastparse.noApi._
import sta.common.Uses
import sta.model.triggers.ModelTrigger
import sta.model.triggers.Implicits._

object CalendarRules extends TriggerParser[CalendarEvent] {
  import CalendarEvent._
  import white._

  def Prefix: String = Uses.categoryOf[CalendarEvent]

  override val Suffix: Option[P[ModelTrigger[_ <: CalendarEvent]]] = Some(
    mapParser(State.namesToValuesMap) map (v => ModelTrigger[CalendarEvent](_.state == v))
  )

  private def title: P[ModelTrigger[CalendarEvent]] = {
    "title" ~ matchStringParser[CalendarEvent](_.title)
  }

  private def description: P[ModelTrigger[CalendarEvent]] = {
    "description" ~ matchStringParser[CalendarEvent](_.description)
  }

  private def location: P[ModelTrigger[CalendarEvent]] = {
    "location" ~ matchStringParser[CalendarEvent](_.location)
  }

  private def availability: P[ModelTrigger[CalendarEvent]] = {
    "availability" ~ "is" ~ mapParser(Availability.namesToValuesMap) map (v =>
      ModelTrigger[CalendarEvent](_.availability == v)
    )
  }

  val Rule: P[ModelTrigger[_ <: CalendarEvent]] = title | description | location | availability
}
