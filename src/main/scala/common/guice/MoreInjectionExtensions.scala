package common.guice

import com.google.inject.{Injector, Provider}

import scala.reflect.ClassTag

import common.rich.primitives.RichClass._

//noinspection UnitMethodIsParameterless
object MoreInjectionExtensions {
  implicit class RichInjector(private val $ : Injector) extends AnyVal {
    def provider[A: ClassTag]: Provider[A] = $.getProvider(implicitly[ClassTag[A]].unerasedClass)
  }
}
