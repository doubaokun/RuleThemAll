package sta.parser.triggers

import fastparse.noApi._
import sta.common.UsedFeatures
import sta.model.triggers.AtomicTrigger
import sta.model.triggers.Implicits._

object CalendarRules extends TriggerParser[CalendarEvent] {
  import CalendarEvent._
  import white._

  def Prefix: String = implicitly[UsedFeatures[CalendarEvent]].category

  override val Suffix: Option[P[AtomicTrigger[_ <: CalendarEvent]]] = Some(
    mapParser(State.namesToValuesMap) map (v => AtomicTrigger[CalendarEvent](_.state == v))
  )

  private def name: P[AtomicTrigger[CalendarEvent]] = {
    "name" ~ matchStringParser[CalendarEvent](_.name)
  }

  private def description: P[AtomicTrigger[CalendarEvent]] = {
    "description" ~ matchStringParser[CalendarEvent](_.description)
  }

  private def availability: P[AtomicTrigger[CalendarEvent]] = {
    "availability" ~ mapParser(Availability.namesToValuesMap) map (v =>
      AtomicTrigger[CalendarEvent](_.availability == v)
    )
  }

  val Rule: P[AtomicTrigger[_ <: CalendarEvent]] = name | description | availability
}
