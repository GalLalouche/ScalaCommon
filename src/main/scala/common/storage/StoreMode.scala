package common.storage

sealed trait StoreMode
object StoreMode {
  case object Replace extends StoreMode
  case object Update extends StoreMode
}
