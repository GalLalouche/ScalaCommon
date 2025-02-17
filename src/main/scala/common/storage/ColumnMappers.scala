package common.storage

import java.time.{LocalDate, LocalDateTime}

import enumeratum.EnumEntry
import slick.jdbc.{JdbcProfile, JdbcType}

import scala.reflect.ClassTag

import common.rich.RichTime.{RichLocalDateTime, RichLong}

class ColumnMappers(implicit d: JdbcProfile) {
  import d.api._

  implicit def enumColumn[E <: Enum[E]: ClassTag]: JdbcType[E] =
    MappedColumnType.base[E, String](_.name, EnumUtils.valueOf[E])
  def enumeratumColumn[A <: EnumEntry: ClassTag](enum: enumeratum.Enum[A]): JdbcType[A] =
    MappedColumnType.base[A, String](_.entryName, enum.withName)
  implicit val localDateTimeColumn: JdbcType[LocalDateTime] =
    MappedColumnType.base[LocalDateTime, Long](_.toMillis, _.toLocalDateTime)
  implicit val localDateColumn: JdbcType[LocalDate] =
    MappedColumnType.base[LocalDate, Long](_.toEpochDay, LocalDate.ofEpochDay)
}
