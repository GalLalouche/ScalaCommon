package common.storage

import slick.ast.BaseTypedType

import scala.concurrent.ExecutionContext

abstract class SlickSingleKeyColumnStorageTemplate[Key, Value](implicit ec: ExecutionContext) extends
    SlickStorageTemplate[Key, Value] {
  import profile.api._

  protected type Id
  protected implicit def btt: BaseTypedType[Id]
  protected def extractId(k: Key): Id
  protected def toId(et: EntityTable): Rep[Id]
  override protected def keyFilter(k: Key)(e: EntityTable): Rep[Boolean] = toId(e) === extractId(k)
}
