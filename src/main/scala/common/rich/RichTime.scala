package common.rich

import java.time._

import scala.math.Ordered.orderingToOrdered

import common.rich.primitives.RichBoolean.richBoolean

object RichTime {
  implicit val OrderingLocalDateTime: Ordering[LocalDateTime] = Ordering.by(_.toMillis)
  implicit val OrderingLocalDate: Ordering[LocalDate] = Ordering.by(_.toEpochDay)

  implicit class RichInstant(private val $ : Instant) extends AnyVal {
    def toLocalDateTime: LocalDateTime = LocalDateTime.from($.atZone(ZoneId.systemDefault))
    def toLocalDate: LocalDate = LocalDate.from($.atZone(ZoneId.systemDefault))
  }
  implicit class RichLong(private val $ : Long) extends AnyVal {
    def toLocalDateTime: LocalDateTime = Instant.ofEpochMilli($).toLocalDateTime
  }
  implicit class RichClock(private val $ : Clock) extends AnyVal {
    def getLocalDateTime: LocalDateTime = $.instant.atZone(ZoneId.systemDefault).toLocalDateTime
    def getLocalDate: LocalDate = $.instant.toLocalDate
  }
  implicit class RichLocalDateTime(private val $ : LocalDateTime) extends AnyVal {
    def toMillis: Long = $.atZone(ZoneId.systemDefault).toInstant.toEpochMilli
    def age(c: Clock): Duration = Duration.between($, c.getLocalDateTime)
    def isNewerThan(d: Duration, c: Clock): Boolean = age(c) < d
    def isOlderThan(d: Duration, c: Clock): Boolean = isNewerThan(d, c).isFalse
  }
}
