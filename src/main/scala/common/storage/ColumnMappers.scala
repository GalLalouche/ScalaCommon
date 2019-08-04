package common.storage

import slick.jdbc.{JdbcProfile, JdbcType}

import scala.collection.BuildFrom
import scala.language.higherKinds
import scala.reflect.ClassTag

class ColumnMappers(implicit d: JdbcProfile) {
  import d.api._

  implicit def enumColumn[E <: Enum[E] : ClassTag]: JdbcType[E] = {
    lazy val values = EnumUtils.values
    MappedColumnType.base[E, Int](_.ordinal, values.apply)
  }
  implicit def iterable[T, F[X] <: Iterable[X]](implicit ssEv: StringSerializable[T],
      fromSeq: BuildFrom[Seq[T], T, F[T]],
      ct1: ClassTag[F[T]],
      ct2: ClassTag[T],
  ): JdbcType[F[T]] =
    MappedColumnType.base[F[T], String](
      value => value.toSeq.map(ssEv.stringify).mkString(ssEv.separator),
      s => {
        val builder = fromSeq.newBuilder(Array[T]())
        val elements = if (s.nonEmpty) s.split(ssEv.separator).map(ssEv.parse) else Array[T]()
        builder.addAll(elements).result()
      })
}
