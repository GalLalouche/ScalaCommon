package common.storage

import slick.jdbc.{JdbcProfile, JdbcType}

import scala.reflect.ClassTag

class ColumnMappers(implicit d: JdbcProfile) {
  import d.api._

  implicit def enumColumn[E <: Enum[E] : ClassTag]: JdbcType[E] = {
    lazy val values = EnumUtils.values
    MappedColumnType.base[E, Int](_.ordinal, e => values.apply(e))
  }
  implicit def sequence[T](implicit ssEv: StringSerializable[T]): JdbcType[Seq[T]] =
    MappedColumnType.base[Seq[T], String](_.map(ssEv.stringify).mkString(ssEv.separator),
      s => if (s.isEmpty) Nil else s.split(ssEv.separator).map(ssEv.parse))
}
