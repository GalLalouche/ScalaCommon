package common.guice

import com.google.inject.Guice
import net.codingwell.scalaguice.InjectorExtensions.ScalaInjector
import net.codingwell.scalaguice.ScalaModule
import org.scalatest.freespec.AnyFreeSpec

import common.test.AuxSpecs

class ModuleUtilsTest extends AnyFreeSpec with AuxSpecs {
  "toLazyInstance" in {
    var counter = 0
    val inj = Guice.createInjector(new ScalaModule with ModuleUtils {
      override def configure(): Unit =
        bind[Int].toLazyInstance {
          counter += 1
          counter
        }
    })
    counter shouldReturn 0
    inj.instance[Int] shouldReturn 1
    counter shouldReturn 1
    inj.instance[Int] shouldReturn 1
    counter shouldReturn 1
  }
}
