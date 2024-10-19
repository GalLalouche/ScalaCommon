package common.storage

import scala.concurrent.{ExecutionContext, Future}

import common.rich.func.BetterFutureInstances._
import common.rich.func.MoreTraverseInstances._
import common.rich.func.RichOptionT._
import scalaz.OptionT
import scalaz.syntax.bind._
import scalaz.syntax.traverse.ToTraverseOps

/**
 * Provides overrides since the TableUtils trait can't have implicit parameters. Like the super
 * class, this class does not make any assumption on the specific DAL implementation, e.g., if it's
 * relational or not. Unless you have a good reason, you probably want to implement this class and
 * not [[Storage]].
 */
abstract class StorageTemplate[Key, Value](implicit ec: ExecutionContext)
    extends Storage[Key, Value] {
  /** If a previous value exists, override it. */
  protected def internalDelete(k: Key): Future[_]
  protected def internalUpdate(k: Key, v: Value): Future[_]
  protected def internalReplace(k: Key, v: Value): Future[_]

  override def update(k: Key, v: Value) =
    OptionT(load(k).run.flatMap {
      case Some(value) => internalUpdate(k, v) >| Some(value)
      case None => internalReplace(k, v) >| None
    })
  override def replace(k: Key, v: Value): OptionT[Future, Value] =
    OptionT(load(k).run.`<*ByName`(internalReplace(k, v)))

  override def store(k: Key, v: Value) = storeMultiple(Vector(k -> v))
  override def overwriteMultipleVoid(kvs: Seq[(Key, Value)]): Future[Unit] =
    kvs.traverse(Function.tupled(internalReplace)).void
  override def mapStore(mode: StoreMode, k: Key, f: Value => Value, default: => Value) = for {
    value <- load(k).map(f).|(default).liftSome
    storer = mode match {
      case StoreMode.Replace => replace _
      case StoreMode.Update => update _
    }
    result <- storer(k, value)
  } yield result

  override def delete(k: Key) = load(k).`<*ByName`(internalDelete(k).liftSome)
  override def exists(k: Key): Future[Boolean] = load(k).isDefined
}
