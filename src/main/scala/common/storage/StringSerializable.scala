package common.storage

import scala.reflect.ClassTag

trait StringSerializable[T] {
  def stringify(t: T): String
  def parse(s: String): T
  /**
   * Used for converting collections of StringSerializables to a string string. Override if your
   * serializer outputs ";".
   */
  def separator = ";"
}

object StringSerializable {
  implicit def EnumStringSerializable[E <: Enum[E]: ClassTag]: StringSerializable[E] =
    new StringSerializable[E] {
      private lazy val values: Array[E] = EnumUtils.values
      override def stringify(e: E): String = e.ordinal.toString
      override def parse(s: String): E = values(s.toInt)
    }
  implicit object IntStringSerializable extends StringSerializable[Int] {
    override def stringify(t: Int): String = t.toString
    override def parse(s: String): Int = s.toInt
  }
  implicit object LongStringSerializable extends StringSerializable[Long] {
    override def stringify(t: Long): String = t.toString
    override def parse(s: String): Long = s.toLong
  }
  implicit object DoubleStringSerializable extends StringSerializable[Double] {
    override def stringify(t: Double): String = t.toString
    override def parse(s: String): Double = s.toDouble
  }
}
