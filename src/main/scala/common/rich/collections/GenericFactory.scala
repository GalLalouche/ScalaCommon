package common.rich.collections

import scala.collection.{mutable, IterableFactory}
import scala.language.higherKinds

trait GenericFactory[F[_]] {
  def newBuilder[A]: mutable.Builder[A, F[A]]
}

object GenericFactory {
  def apply[F[_]](ii: IterableFactory[F]): GenericFactory[F] = new GenericFactory[F] {
    override def newBuilder[A] = ii.newBuilder
  }
  implicit val seqEv: GenericFactory[Seq] = apply(Seq)
  implicit val setEv: GenericFactory[Set] = apply(Set)
  implicit val iterableEv: GenericFactory[Iterable] = apply(Iterable)
  implicit val iteratorEv: GenericFactory[Iterator] = apply(Iterator)
}
