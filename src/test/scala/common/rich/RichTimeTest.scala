package common.rich

import java.time.{Clock, Duration, Instant, LocalDateTime, ZoneId}

import org.scalatest.freespec.AnyFreeSpec

import common.rich.RichTime._
import common.test.AuxSpecs

class RichTimeTest extends AnyFreeSpec with AuxSpecs {
  private val clock = Clock.fixed(Instant.ofEpochSecond(2000, 1000), ZoneId.systemDefault())
  "RichLocalDateTime" - {
    "age" in {
      val age = LocalDateTime
        .ofInstant(Instant.ofEpochSecond(1000, 500), ZoneId.systemDefault())
        .age(clock)
      age.getSeconds shouldReturn 1000
      age.getNano shouldReturn 500
    }
    "isNewerThan" in {
      val t1 = LocalDateTime.ofInstant(Instant.ofEpochSecond(1000, 500), ZoneId.systemDefault())
      t1.isNewerThan(Duration.ofMillis(1000), clock) shouldReturn false
    }
  }
}
