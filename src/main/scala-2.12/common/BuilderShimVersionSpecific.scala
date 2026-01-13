package common

trait BuilderShimVersionSpecific[A] { self: scala.collection.mutable.Builder[A, _] =>
  def addOne(elem: A): this.type
  override def +=(elem: A): this.type = addOne(elem)
}
