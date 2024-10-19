package common.storage

import java.time.{LocalDate, LocalDateTime}

import enumeratum.EnumEntry
import slick.jdbc.{JdbcProfile, JdbcType}

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.reflect.ClassTag

import common.rich.RichTime.{RichLocalDateTime, RichLong}

class ColumnMappers(implicit d: JdbcProfile) {
  import d.api._

  implicit def enumColumn[E <: Enum[E]: ClassTag]: JdbcType[E] =
    MappedColumnType.base[E, String](_.name, EnumUtils.valueOf[E])
  def enumeratumColumn[A <: EnumEntry: ClassTag](enum: enumeratum.Enum[A]): JdbcType[A] =
    MappedColumnType.base[A, String](_.entryName, enum.withName)
  implicit def traversable[T, F[X] <: Traversable[X]](implicit
      ssEv: StringSerializable[T],
      fromSeq: CanBuildFrom[Nothing, T, F[T]],
      ct: ClassTag[F[T]],
  ): JdbcType[F[T]] =
    MappedColumnType.base[F[T], String](
      value => value.toSeq.map(ssEv.stringify).mkString(ssEv.separator),
      s => {
        val builder = fromSeq.apply()
        if (s.nonEmpty)
          s.split(ssEv.separator).map(ssEv.parse).foreach(builder.+=)
        builder.result()
      },
    )
  implicit val localDateTimeColumn: JdbcType[LocalDateTime] =
    MappedColumnType.base[LocalDateTime, Long](_.toMillis, _.toLocalDateTime)
  implicit val localDateColumn: JdbcType[LocalDate] =
    MappedColumnType.base[LocalDate, Long](_.toEpochDay, LocalDate.ofEpochDay)
}
