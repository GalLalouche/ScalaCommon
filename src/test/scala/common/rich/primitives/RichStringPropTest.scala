package common.rich.primitives

import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import common.rich.primitives.RichBoolean.richBoolean
import common.rich.primitives.RichString._
import common.test.AuxSpecs
import common.test.ScalaCheckTypes.binaryString

class RichStringPropTest extends PropSpec with GeneratorDrivenPropertyChecks with AuxSpecs {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)
  property("longestCommonSuffix is a common suffix") {
    forAll(binaryString, binaryString) {(s1, s2) =>
      val result = s1.longestCommonSuffix(s2)
      s1 should endWith(result)
      s2 should endWith(result)
    }
  }
  property("longestCommonSuffix is the longest common suffix") {
    forAll(binaryString, binaryString) {(s1, s2) =>
      val result = s1.longestCommonSuffix(s2)
      if (s1.length > result.length) {
        val alternative = s1.takeRight(result.length + 1)
        assert(
          s2.endsWith(alternative).isFalse,
          s"longestCommonSuffix was '$result', but '$alternative' is a longer match",
        )
      }
    }
  }
}
