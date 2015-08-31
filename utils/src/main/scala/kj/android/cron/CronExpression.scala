package kj.android.cron

import java.util.{Arrays, Calendar, Date}

case class CronExpression(minute: CronExpression.Value, hour: CronExpression.Value,
  dayOfMonth: CronExpression.Value, month: CronExpression.Value,
  dayOfWeek: CronExpression.Value, year: Option[CronExpression.Value]) {
  require(minute.min >= 0 && minute.max <= 59)
  require(hour.min >= 0 && hour.max <= 23)
  require(dayOfMonth.min >= 1 && dayOfMonth.max <= 31)
  require(month.min >= 1 && month.max <= 12)
  require(dayOfWeek.min >= 0 && dayOfWeek.max <= 6)
  require(year.forall(y => y.min >= 1970 && y.max <= 2099))

  private def adjustDate(calendar: Calendar)(field: Int,
    value: CronExpression.Value, includeSelf: Boolean, bias: Int): Boolean = {
    value.next(calendar.get(field) - bias, includeSelf = includeSelf).fold {
      calendar.set(field, value.min + bias)
      false
    } { v =>
      calendar.set(field, v + bias)
      true
    }
  }

  def nextDate(from: Date): Option[Date] = {
    val calendar = Calendar.getInstance()
    calendar.setTime(from)
    val adjust = adjustDate(calendar) _
    val includeSelf1 = adjust(Calendar.MINUTE, minute, true, 0)
    val includeSelf2 = adjust(Calendar.HOUR_OF_DAY, hour, includeSelf1, 0)
    val includeSelf3 = adjust(Calendar.DAY_OF_MONTH, dayOfMonth, includeSelf2, 0)
    val includeSelf4 = adjust(Calendar.MONTH, month, includeSelf3, -1)
    val includeSelf5 = adjust(Calendar.DAY_OF_WEEK, dayOfWeek, includeSelf4, 1)
    val success = year.forall(adjust(Calendar.YEAR, _, includeSelf5, 0))
    if (success) Some(calendar.getTime) else None
  }
}

object CronExpression {
  sealed abstract class Value {
    def min: Int

    def max: Int
    
    def next(from: Int, includeSelf: Boolean): Option[Int]
  }

  case class List(min: Int, rest: Array[Int]) extends Value {
    def max: Int = rest.headOption.getOrElse(min)

    def next(from: Int, includeSelf: Boolean) = from match {
      case `min` =>
        if (includeSelf) Some(min)
        else rest.headOption
      case _ =>
        if (from < min || rest.isEmpty || from > rest.last ||
          (!includeSelf && from == rest.last)) None
        else {
          val idx = Arrays.binarySearch(rest, from)
          if (idx >= 0) Some(if (includeSelf) rest(idx) else rest(idx + 1))
          else Some(rest(Math.abs(idx) - 1))
        }
    }
  }

  case class Range(min: Int, max: Int, step: Int = 1) extends Value {
    def next(from: Int, includeSelf: Boolean) = {
      if (from < min || from > max || (!includeSelf && from == max)) None
      else if ((from - min) % step == 0) {
        if (includeSelf) Some(from) else Some(from + step)
      } else {
        val length = (max - min) / step + 1
        val idx = length - ((min + length * step) - from) / step
        Some(min + idx * step)
      }
    }
  }
}
