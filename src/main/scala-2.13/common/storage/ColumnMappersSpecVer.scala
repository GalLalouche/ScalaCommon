package common.storage

import slick.jdbc.{JdbcProfile, JdbcType}

import scala.collection.{Factory, IterableFactoryDefaults}
import scala.reflect.ClassTag

/**
 * Returns a Future of a collection of all elements emitted by this observable. Future will not
 * complete if this observable does not complete.
 */
class ColumnMappersSpecVer(implicit d: JdbcProfile) {
  import d.api._

  implicit def iterable[T, F[X] <: IterableFactoryDefaults[X, F]](implicit
      ssEv: StringSerializable[T],
      factory: Factory[T, F[T]],
      cf: ClassTag[F[T]],
      ct: ClassTag[T],
  ): JdbcType[F[T]] =
    MappedColumnType.base[F[T], String](
      value => value.toSeq.map(ssEv.stringify).mkString(ssEv.separator),
      s => {
        val builder = factory.newBuilder
        if (s.nonEmpty)
          s.split(ssEv.separator).map(ssEv.parse).foreach(builder.+=)
        builder.result()
      },
    )
}
