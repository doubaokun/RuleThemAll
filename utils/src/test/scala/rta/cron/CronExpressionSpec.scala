package rta.cron

import java.util.{Date, GregorianCalendar}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import scala.util.Random

class CronExpressionSpec extends FlatSpec with PropertyChecks with Matchers {
  behavior of "CronExpression.List"

  it should "return None for out of range starting point" in {
    forAll(Gen.nonEmptyListOf(Gen.chooseNum(0, Int.MaxValue - 1))) { xs: List[Int] =>
      val sorted = xs.sorted.distinct.toArray
      val greater = Random.nextInt(Int.MaxValue) + sorted.last
      val list = CronExpression.List(min = sorted.head, rest = sorted.tail)
      list.next(greater, includeSelf = true) shouldBe empty
      list.next(sorted.last, includeSelf = false) shouldBe empty
    }
  }

  it should "return proper next value" in {
    forAll(Gen.nonEmptyListOf(Gen.chooseNum(0, Int.MaxValue))) { xs: List[Int] =>
      val sorted = xs.sorted.toArray
      val list = CronExpression.List(min = sorted.head, rest = sorted.tail)
      xs.foreach(x => list.next(x, includeSelf = true) shouldBe Some(x))
    }

    val list = CronExpression.List(min = 0, rest = Array(1, 15, 16, 29, 45, 56, 78, 92))
    list.next(1, includeSelf = false) shouldBe Some(15)
    list.next(2, includeSelf = false) shouldBe Some(15)
    list.next(17, includeSelf = false) shouldBe Some(29)
    list.next(42, includeSelf = false) shouldBe Some(45)
    list.next(55, includeSelf = false) shouldBe Some(56)
    list.next(72, includeSelf = false) shouldBe Some(78)
    list.next(83, includeSelf = false) shouldBe Some(92)
  }

  behavior of "CronExpression.Range"

  it should "return None for out of range starting point" in {
    forAll(Gen.chooseNum(0, Int.MaxValue - 1), Gen.chooseNum(1, Int.MaxValue - 1)) { (v1: Int, v2: Int) =>
      val min = math.min(v1, v2)
      val max = if (v1 == v2) v1 + 1 else math.max(v1, v2)
      val greater = Random.nextInt(Int.MaxValue) + max
      val step = ((Random.nextInt(max) + min) * 0.1).toInt + 1
      val range = CronExpression.Range(min = min, max = max, step = step)
      range.next(greater, includeSelf = true) shouldBe empty
      range.next(max, includeSelf = false) shouldBe empty
    }
  }

  it should "return proper next value" in {
    forAll(Gen.chooseNum(0, Int.MaxValue), Gen.chooseNum(1, Int.MaxValue - 1)) { (v1: Int, v2: Int) =>
      val min = math.min(v1, v2)
      val max = if (v1 == v2) v1 + 1 else math.max(v1, v2)
      val step = ((Random.nextInt(max) + min) * 0.1).toInt + 1
      val range = CronExpression.Range(min = min, max = max, step = step)
      (min to max by step).foreach(x => range.next(x, includeSelf = true) shouldBe Some(x))
    }

    val range = CronExpression.Range(min = 0, max = 59, step = 11)
    range.next(0, includeSelf = false) shouldBe Some(11)
    range.next(5, includeSelf = false) shouldBe Some(11)
    range.next(17, includeSelf = false) shouldBe Some(22)
    range.next(32, includeSelf = false) shouldBe Some(33)
    range.next(34, includeSelf = false) shouldBe Some(44)
    range.next(49, includeSelf = false) shouldBe Some(55)
  }

  behavior of "CronExpression"

  it should "return None for out of range starting point" in {
    val expr = CronExpression(
      minute = CronExpression.Range(0, 59),
      hour = CronExpression.Range(0, 23),
      dayOfMonth = CronExpression.Range(1, 31),
      month = CronExpression.Range(1, 12),
      dayOfWeek = CronExpression.Range(0, 6),
      year = Some(CronExpression.List(2000, Array.empty))
    )

    expr.nextDate(new Date) shouldBe empty
  }

  it should "return proper next value" in {
    val expr = CronExpression(
      minute = CronExpression.Range(0, 59),
      hour = CronExpression.Range(0, 11),
      dayOfMonth = CronExpression.Range(1, 31),
      month = CronExpression.Range(1, 12),
      dayOfWeek = CronExpression.Range(0, 6),
      year = Some(CronExpression.Range(1970, 2099))
    )

    val date = new GregorianCalendar(2015, 6, 15, 12, 0, 30).getTime
    expr.nextDate(date) shouldBe Some(new GregorianCalendar(2015, 6, 16, 0, 0, 30).getTime)
  }
}
