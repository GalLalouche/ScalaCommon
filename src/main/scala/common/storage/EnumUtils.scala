package common.storage

import scala.reflect.ClassTag

private object EnumUtils {
  def values[E <: Enum[E] : ClassTag]: Array[E] = {
    val clazz = implicitly[ClassTag[E]].runtimeClass
    val method = clazz.getMethod("values")
    method.invoke(null).asInstanceOf[Array[E]]
  }
  def valueOf[E <: Enum[E] : ClassTag](s: String) = {
    val clazz = implicitly[ClassTag[E]].runtimeClass
    val method = clazz.getMethod("valueOf", classOf[String])
    method.invoke(null, s).asInstanceOf[E]
  }
}
