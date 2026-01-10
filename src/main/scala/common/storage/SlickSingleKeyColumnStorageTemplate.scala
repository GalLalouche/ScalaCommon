package common.storage

import slick.ast.BaseTypedType

import scala.concurrent.{ExecutionContext, Future}

abstract class SlickSingleKeyColumnStorageTemplate[Key, Value](implicit ec: ExecutionContext)
    extends SlickStorageTemplate[Key, Value] {
  import profile.api._

  protected type Id
  protected implicit def btt: BaseTypedType[Id]
  protected def extractId(k: Key): Id
  protected def toId(et: EntityTable): Rep[Id]
  // May fail if the key list is too large, depending on the DB engine.
  override def deleteAll(ks: Iterable[Key]): Future[Int] =
    db.run(tableQuery.filter(toId(_).inSet(ks.map(extractId))).delete)
  protected override def keyFilter(k: Key)(e: EntityTable): Rep[Boolean] = toId(e) === extractId(k)
}
