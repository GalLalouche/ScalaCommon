package common.storage

import scala.reflect.ClassTag

trait StringSerializable[T] {
  def stringify(t: T): String
  def parse(s: String): T
  // Override this if your serializer outputs ";"
  def separator = ";"
}

object StringSerializable {
  implicit def EnumStringSerializable[E <: Enum[E] : ClassTag]: StringSerializable[E] = new StringSerializable[E] {
    private lazy val values: Array[E] = EnumUtils.values
    override def stringify(e: E): String = e.ordinal.toString
    override def parse(s: String): E = values(s.toInt)
  }
}
