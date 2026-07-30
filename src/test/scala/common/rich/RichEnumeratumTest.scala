package common.rich

import org.scalatest.freespec.AnyFreeSpec

import common.rich.RichEnumeratumTest.TestEnum
import common.test.AuxSpecs

class RichEnumeratumTest extends AnyFreeSpec with AuxSpecs {
  import RichEnumeratum.richEnumeratum
  import RichEnumeratumTest.TestEnum._

  "withPrefixCaseInsensitive" in {
    TestEnum.withPrefixCaseInsensitive("a") shouldReturn Vector(A)
    TestEnum.withPrefixCaseInsensitive("A") shouldReturn Vector(A)
    TestEnum.withPrefixCaseInsensitive("") shouldReturn Vector(A, B, C, Dingo, Dingbat)
    TestEnum.withPrefixCaseInsensitive("ding") shouldReturn Vector(Dingo, Dingbat)
    TestEnum.withPrefixCaseInsensitive("DiNgO") shouldReturn Vector(Dingo)
  }
  "ordering" in {
    implicit val ordering: Ordering[TestEnum] = TestEnum.ordering
    val expected = Vector(A, A, A, A, B, B, B, B, C, C, C, C, Dingo)
    Vector[TestEnum](C, B, A, B, A, A, Dingo, B, C, C, B, C, A).sorted shouldReturn expected
  }
}

object RichEnumeratumTest {
  sealed trait TestEnum extends enumeratum.EnumEntry
  object TestEnum extends enumeratum.Enum[TestEnum] {
    val values = findValues
    case object A extends TestEnum
    case object B extends TestEnum
    case object C extends TestEnum
    case object Dingo extends TestEnum
    case object Dingbat extends TestEnum
  }
}
