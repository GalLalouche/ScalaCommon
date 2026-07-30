package common.rich

import java.nio.file.attribute.FileTime
import java.time._

import scala.concurrent.duration
import scala.math.Ordered.orderingToOrdered

object RichTime {
  implicit val OrderingLocalDateTime: Ordering[LocalDateTime] =
    Ordering.comparatorToOrdering(_.compareTo(_))
  implicit val OrderingLocalDate: Ordering[LocalDate] = Ordering.by(_.toEpochDay)

  implicit class RichInstant(private val $ : Instant) extends AnyVal {
    def toLocalDateTime(c: Clock): LocalDateTime = LocalDateTime.from($.atZone(c.getZone))
    def toLocalDate(c: Clock): LocalDate = LocalDate.from($.atZone(c.getZone))
    def minus(d: duration.Duration): Instant = $.minusMillis(d.toMillis.toInt)
    def plus(d: duration.Duration): Instant = $.plusMillis(d.toMillis.toInt)
  }
  implicit class RichLong(private val $ : Long) extends AnyVal {
    def toLocalDateTime(c: Clock): LocalDateTime = Instant.ofEpochMilli($).toLocalDateTime(c)
  }
  implicit class RichClock(private val $ : Clock) extends AnyVal {
    def getLocalDateTime: LocalDateTime = $.instant.toLocalDateTime($)
    def getLocalDate: LocalDate = $.instant.toLocalDate($)
  }
  implicit class RichLocalDateTime(private val $ : LocalDateTime) extends AnyVal {
    def toMillis(clock: Clock): Long = $.atZone(clock.getZone).toInstant.toEpochMilli
    def toInstant(clock: Clock): Instant = $.atZone(clock.getZone).toInstant
    def age(c: Clock): Duration = Duration.between($, c.getLocalDateTime)
    def isNewerThan(d: Duration, c: Clock): Boolean = age(c) < d
    def isOlderThan(d: Duration, c: Clock): Boolean = age(c) > d
  }
  implicit class RichFileTime(private val $ : FileTime) extends AnyVal {
    def toLocalDateTime(c: Clock): LocalDateTime = $.toInstant.toLocalDateTime(c)
  }
}
