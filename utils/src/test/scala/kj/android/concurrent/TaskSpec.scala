package kj.android.concurrent

import java.util.concurrent.atomic.AtomicInteger
import kj.android.cron.CronExpression
import org.scalatest.tagobjects.Slow
import org.scalatest.{Matchers, FlatSpec}
import scala.concurrent.duration._

class TaskSpec extends FlatSpec with Matchers {
  "Task.apply" should "fire task once right after call to run" in {
    val bodyCounter = new AtomicInteger(0)
    val handlerCounter = new AtomicInteger(0)
    val task = Task {
      bodyCounter.incrementAndGet()
    }

    task.run(_ => handlerCounter.incrementAndGet())
    Thread.sleep(50)
    bodyCounter.get() should === (1)
    handlerCounter.get() should === (1)
  }

  "Task.schedule" should "fire task repeatedly after specified delay" in {
    val bodyCounter = new AtomicInteger(0)
    val handlerCounter = new AtomicInteger(0)
    val task = Task.schedule(100.millis, 250.millis) {
      bodyCounter.incrementAndGet()
    }

    task.run(_ => handlerCounter.incrementAndGet())
    Thread.sleep(800)
    task.cancel(true)
    bodyCounter.get() should === (3)
    handlerCounter.get() should === (3)
  }

  "Task.scheduleOnce" should "fire task once after specified delay" in {
    val bodyCounter = new AtomicInteger(0)
    val handlerCounter = new AtomicInteger(0)
    val task = Task.scheduleOnce(100.millis) {
      bodyCounter.incrementAndGet()
    }

    task.run(_ => handlerCounter.incrementAndGet())
    Thread.sleep(50)
    task.cancel(true)
    bodyCounter.get() should === (0)
    handlerCounter.get() should === (0)

    task.run(_ => handlerCounter.incrementAndGet())
    Thread.sleep(1000)
    bodyCounter.get() should === (1)
    handlerCounter.get() should === (1)
  }

  "Task.repeat" should "fire task repeatedly on every date that matches cron expression" taggedAs Slow in {
    val bodyCounter = new AtomicInteger(0)
    val handlerCounter = new AtomicInteger(0)
    val expr = CronExpression(
      minute = CronExpression.Range(0, 59),
      hour = CronExpression.Range(0, 23),
      dayOfMonth = CronExpression.Range(1, 31),
      month = CronExpression.Range(1, 12),
      dayOfWeek = CronExpression.Range(0, 6),
      year = None
    )
    val task = Task.repeat(expr) {
      bodyCounter.incrementAndGet()
    }

    task.run(_ => handlerCounter.incrementAndGet())
    Thread.sleep(1000)
    task.cancel(true)
    bodyCounter.get() should === (1)
    handlerCounter.get() should === (1)

    task.run(_ => handlerCounter.incrementAndGet())
    Thread.sleep(61000)
    task.cancel(true)
    bodyCounter.get() should === (3)
    handlerCounter.get() should === (3)
  }

  "Task.join" should "join multiple tasks" in {
    val bodyCounter = new AtomicInteger(0)
    val handlerCounter = new AtomicInteger(0)
    val task = Task.join(Seq(
      Task {
        bodyCounter.incrementAndGet()
      },
      Task.scheduleOnce(200.millis) {
        bodyCounter.incrementAndGet()
      },
      Task.scheduleOnce(400.millis) {
        bodyCounter.incrementAndGet()
      }
    ))

    task.run(_ => handlerCounter.incrementAndGet())
    Thread.sleep(50)
    bodyCounter.get() should === (1)
    handlerCounter.get() should === (1)
    Thread.sleep(200)
    bodyCounter.get() should === (2)
    handlerCounter.get() should === (2)
    task.cancel(true)
    Thread.sleep(200)
    bodyCounter.get() should === (2)
    handlerCounter.get() should === (2)
  }
}