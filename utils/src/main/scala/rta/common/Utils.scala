package rta.common

import android.os.RemoteException
import java.util.concurrent.locks.Lock

object Utils {
  @inline def inLock[@specialized(Unit) T](lock: Lock)(body: => T): T = {
    lock.lock()
    try {
      body
    } finally {
      lock.unlock()
    }
  }

  @inline def withGC[@specialized(Unit) T](body: => T) = {
    val result = body
    val v = System.getProperty("java.vm.version")
    if (v != null && v >= "2.0.0") System.gc()
    result
  }

  @inline def remote(onError: => Unit)(body: => Unit) = try {
    body
  } catch {
    case ex: RemoteException => onError
  }
}
