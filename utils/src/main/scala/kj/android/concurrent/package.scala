package kj.android

import java.util.concurrent.{Executors, ThreadFactory}

package object concurrent {
  private[concurrent] val scheduledExecutorService =
    Executors.newScheduledThreadPool(1, new ThreadFactory {
      val defaultThreadFactory = Executors.defaultThreadFactory()

      def newThread(r: Runnable) = {
        val t = defaultThreadFactory.newThread(r)
        t.setDaemon(true)
        t
      }
    })
}
