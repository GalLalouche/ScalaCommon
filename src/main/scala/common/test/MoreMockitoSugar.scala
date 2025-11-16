package common.test

import org.mockito.{ArgumentCaptor, Mockito}
import org.mockito.verification.VerificationMode
import org.scalatest.{Assertion, Succeeded}

import common.rich.primitives.RichClass.richClassTag
import common.test.MoreMockitoSugar.Capturer

trait MoreMockitoSugar {
  def doVerify[A](a: A)(f: A => Any): Assertion = {
    f(Mockito.verify(a))
    Succeeded
  }
  def doVerify[A](a: A, verificationMode: VerificationMode)(f: A => Any): Assertion = {
    f(Mockito.verify(a, verificationMode))
    Succeeded
  }
  def argumentCaptor[A: Manifest]: ArgumentCaptor[A] =
    ArgumentCaptor.forClass(manifest.unerasedClass)
  /** Usage: `capture[CapturedType](mock)(_.function(_))` */
  def capture[C]: Capturer[C] = new Capturer[C]
}

object MoreMockitoSugar extends MoreMockitoSugar {
  /** This is a pretty convoluted way of allowing the above syntax for [[capture]]. */
  class Capturer[C] private[MoreMockitoSugar] {
    def apply[A](mock: A)(f: (A, C) => Any)(implicit manifest: Manifest[C]): C = {
      val argument = ArgumentCaptor.forClass(manifest.unerasedClass)
      f(Mockito.verify(mock), argument.capture())
      argument.getValue
    }
  }
}
