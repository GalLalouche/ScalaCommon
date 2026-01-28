package common.test.memory_ref

import java.nio.file.attribute.FileTime
import java.time.{LocalDateTime, ZoneOffset}

private object FileTimeUtils {
  def from(localDateTime: LocalDateTime): FileTime =
    FileTime.from(localDateTime.toInstant(ZoneOffset.UTC))
}
