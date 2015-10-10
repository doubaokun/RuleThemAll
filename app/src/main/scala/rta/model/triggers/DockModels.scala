package rta.model.triggers

import android.content.Intent
import enumeratum.Enum
import rta.common.{action, category}
import rta.model._

trait DockModels {
  @category("dock")
  @action(Intent.ACTION_DOCK_EVENT)
  sealed abstract class DockState extends Model[DockState](DockState) with FromIntEntry

  implicit object DockState extends ModelCompanion[DockState] with Enum[DockState] with FromInt[DockState] {
    trait DockType extends FromIntEntry {
      def isDesk: Boolean
    }

    lazy val values = findValues

    case object Disconnected extends DockState {
      def intValue: Int = Intent.EXTRA_DOCK_STATE_UNDOCKED
    }

    case object CarConnected extends DockState with DockType {
      def intValue: Int = Intent.EXTRA_DOCK_STATE_CAR

      def isDesk: Boolean = false
    }

    case object DeskConnected extends DockType {
      def intValue: Int = Intent.EXTRA_DOCK_STATE_DESK

      def isDesk: Boolean = true
    }

    case object AnalogDeskConnected extends DockType {
      def intValue: Int = Intent.EXTRA_DOCK_STATE_LE_DESK

      def isDesk: Boolean = true
    }

    case object DigitalDeskConnected extends DockType {
      def intValue: Int = Intent.EXTRA_DOCK_STATE_HE_DESK

      def isDesk: Boolean = true
    }
  }
}
