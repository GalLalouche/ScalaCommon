package common.storage

import slick.jdbc.{JdbcProfile, JdbcType}

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.reflect.ClassTag

class ColumnMappers(implicit d: JdbcProfile) {
  import d.api._

  implicit def enumColumn[E <: Enum[E] : ClassTag]: JdbcType[E] = {
    lazy val values = EnumUtils.values
    MappedColumnType.base[E, Int](_.ordinal, values.apply)
  }
  implicit def traversable[T, F[X] <: Traversable[X]](implicit ssEv: StringSerializable[T],
      fromSeq: CanBuildFrom[Nothing, T, F[T]],
      ct: ClassTag[F[T]]): JdbcType[F[T]] =
    MappedColumnType.base[F[T], String](
      value => value.toSeq.map(ssEv.stringify).mkString(ssEv.separator),
      s => {
        val builder = fromSeq.apply()
        if (s.nonEmpty)
          s.split(ssEv.separator).map(ssEv.parse).foreach(builder.+=)
        builder.result()
      })
}
