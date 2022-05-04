package common.guice

import com.google.inject.Module
import com.google.inject.util.Modules

//noinspection UnitMethodIsParameterless
object RichModule {
  implicit class richModule(private val $: Module) extends AnyVal {
    def overrideWith(other: Module): Module = Modules.`override`($).`with`(other)
    def overriding(other: Module): Module = Modules.`override`(other).`with`($)
  }
}
