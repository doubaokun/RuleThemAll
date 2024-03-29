package rta.model.actions

import android.content.Context
import android.media.AudioManager
import enumeratum.Enum
import spire.math.UByte
import rta.common.SystemServices._
import rta.model.FromIntEntry

sealed abstract class SetSoundTo extends SetTo { this: Product => }

object SetSoundTo {
  sealed abstract class StreamType extends FromIntEntry
  object StreamType extends Enum[StreamType] {
    lazy val values = findValues
    
    case object VoiceCall extends StreamType {
      def intValue: Int = AudioManager.STREAM_VOICE_CALL
    }
    case object System extends StreamType {
      def intValue: Int = AudioManager.STREAM_SYSTEM
    }
    case object Ring extends StreamType {
      def intValue: Int = AudioManager.STREAM_RING
    }
    case object Music extends StreamType {
      def intValue: Int = AudioManager.STREAM_MUSIC
    }
    case object Alarm extends StreamType {
      def intValue: Int = AudioManager.STREAM_ALARM
    }
    case object Notification extends StreamType {
      def intValue: Int = AudioManager.STREAM_NOTIFICATION
    }
    case object DTMF extends StreamType {
      def intValue: Int = AudioManager.STREAM_DTMF
    }
  }

  final case class Muted(streamType: StreamType, mute: Boolean) extends SetSoundTo {
    def execute()(implicit ctx: Context): Unit = {
      audioManger.setStreamMute(streamType.intValue, mute)
    }
  }

  final case class Volume(streamType: StreamType, value: UByte) extends SetSoundTo {
    def execute()(implicit ctx: Context): Unit = {
      val am = audioManger
      val max = am.getStreamMaxVolume(streamType.intValue)
      am.setStreamVolume(streamType.intValue, value.toInt * max / 100, 0)
    }
  }
}
