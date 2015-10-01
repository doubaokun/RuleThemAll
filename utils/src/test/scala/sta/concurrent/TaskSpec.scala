package sta.concurrent

import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.{FlatSpec, Matchers}
import scala.concurrent.duration._

class TaskSpec extends FlatSpec with Matchers {
  import ExecutionContext.Implicits._

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
}
