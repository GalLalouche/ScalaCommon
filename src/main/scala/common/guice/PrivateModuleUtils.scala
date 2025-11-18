package common.guice

import com.google.inject.{PrivateBinder, PrivateModule}
import net.codingwell.scalaguice.InternalModule

import common.rich.primitives.RichClass.richClassTag

trait PrivateModuleUtils { self: InternalModule[_ <: PrivateBinder] with PrivateModule =>
  private def binder: PrivateBinder = self.binderAccess

  /** Combines [[bind]] with [[expose]]. */
  protected def publicBind[A: Manifest]: BindingBuilder[A] = {
    binder.expose(manifest.unerasedClass)
    new BindingBuilder[A]
  }
}
