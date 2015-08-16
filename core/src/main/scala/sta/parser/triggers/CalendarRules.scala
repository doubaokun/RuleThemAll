package sta.parser.triggers

import fastparse.noApi._
import kj.android.common.UsedFeatures
import sta.model.triggers.AtomicTrigger
import sta.model.triggers.Implicits._
import sta.parser.WhitespaceSkip

object CalendarRules extends TriggerParser[CalendarEvent] with WhitespaceSkip {
  import CalendarEvent._
  import white._

  def prefix: String = implicitly[UsedFeatures[CalendarEvent]].category

  override val Main: Option[P[AtomicTrigger[_ <: CalendarEvent]]] = Some(P(
    mapParser(State.namesToValuesMap) map (v => AtomicTrigger[CalendarEvent](_.state == v))
  ))

  private def name: P[AtomicTrigger[CalendarEvent]] = P(
    "name" ~ matchStringParser[CalendarEvent](_.name)
  )

  private def description: P[AtomicTrigger[CalendarEvent]] = P(
    "description" ~ matchStringParser[CalendarEvent](_.description)
  )

  private def availability: P[AtomicTrigger[CalendarEvent]] = P(
    "availability" ~ mapParser(Availability.namesToValuesMap) map (v =>
      AtomicTrigger[CalendarEvent](_.availability == v)
    )
  )

  val Rule: P[AtomicTrigger[_ <: CalendarEvent]] = P(
    name | description | availability
  )
}
