package kj.android

import android.content.Context
import android.os.PowerManager
import kj.android.logging.LogTag

package object common {
  import SystemServices._

  @inline def inWakeLock(thunk: => Unit)(implicit ctx: Context, logTag: LogTag) = {
    val lock = powerManager.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, logTag.tag)
    try {
      lock.acquire()
      thunk
    } finally {
      lock.release()
    }
  }
}

package common {
  case class AppInfo(name: String, smallIcon: Int)
}
