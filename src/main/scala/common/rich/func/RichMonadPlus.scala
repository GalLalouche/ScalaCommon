package common.rich.func

import scala.util.Try
import scalaz.MonadPlus
import common.rich.RichT._

import scala.reflect.ClassTag

object RichMonadPlus {
  private val primitiveMappings: Map[Class[_], Class[_]] = Map(
    java.lang.Integer.TYPE -> classOf[java.lang.Integer],
    java.lang.Long.TYPE -> classOf[java.lang.Long],
    java.lang.Double.TYPE -> classOf[java.lang.Double],
    java.lang.Float.TYPE -> classOf[java.lang.Float],
    java.lang.Boolean.TYPE -> classOf[java.lang.Boolean],
    java.lang.Character.TYPE -> classOf[java.lang.Character],
    java.lang.Byte.TYPE -> classOf[java.lang.Byte],
    java.lang.Void.TYPE -> classOf[java.lang.Void],
    java.lang.Short.TYPE -> classOf[java.lang.Short])

  implicit class richMonadPlus[A, F[A] : MonadPlus]($: F[A]) {
    def tryMap[B](f: A => B): F[B] =
      implicitly[MonadPlus[F]].map($)(e => Try(f(e)))
        .mapTo(e => implicitly[MonadPlus[F]].filter(e)(_.isSuccess))
        .mapTo(e => implicitly[MonadPlus[F]].map(e)(_.get))

    def select[B <: A : Manifest]: F[B] = {
      val clazz: Class[_] = implicitly[Manifest[B]].runtimeClass
      // Due to scala's own "primitives", e.g., Int vs int, there is a mismatch between the Manifest[B] and
      // the type of the elements in a given collec... err MonadPlus. Therefore, we map the primitive
      // classes to their boxed types.
      val referenceClass = primitiveMappings.getOrElse(clazz, clazz)
      implicitly[MonadPlus[F]].filter($)(e => referenceClass.isAssignableFrom(e.getClass))
        .mapTo(e => implicitly[MonadPlus[F]].map(e)(_.asInstanceOf[B]))
    }
  }

}
