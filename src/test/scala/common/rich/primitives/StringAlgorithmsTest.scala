package common.rich.primitives

import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import common.rich.primitives.RichBoolean.richBoolean
import common.test.AuxSpecs
import common.test.ScalaCheckTypes.BinaryString

class StringAlgorithmsTest extends PropSpec with GeneratorDrivenPropertyChecks with AuxSpecs {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)
  property("longestCommonSuffix is a common suffix") {
    forAll((ss: Seq[BinaryString]) => {
      val strings = ss.map(_.s)
      val result = StringAlgorithms.longestCommonSuffix(strings)
      assertAll(strings.map(_ should endWith(result)))
    })
  }
  property("longestCommonSuffix is the longest common suffix") {
    forAll((ss: Seq[BinaryString]) => {
      val strings = ss.map(_.s)
      val result = StringAlgorithms.longestCommonSuffix(strings)
      strings.find(_.length > result.length).map(_.takeRight(result.length + 1)).foreach(alternative =>
        assert(
          strings.exists(_.endsWith(alternative).isFalse),
          s"longestCommonSuffix was '$result', but '$alternative' is a longer match for $strings",
        )
      )
    })
  }
}
