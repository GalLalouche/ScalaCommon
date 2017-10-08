package common.storage

import scala.reflect.ClassTag

private object EnumUtils {
  def values[E <: Enum[E] : ClassTag] = {
    val clazz = implicitly[ClassTag[E]].runtimeClass
    val method = clazz.getMethod("values")
    method.invoke(null).asInstanceOf[Array[E]]
  }
}
