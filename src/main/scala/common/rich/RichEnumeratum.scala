package common.rich

import enumeratum.{Enum, EnumEntry}
import org.scalacheck.Gen

import scala.util.Random

import common.rich.RichRandomSpecVer.richRandomSpecVer
import common.rich.collections.RichSeq._
import common.rich.primitives.RichOption._
import common.rich.primitives.RichString.richString
import common.test.MoreGen

object RichEnumeratum {
  implicit class richEnumeratum[A <: EnumEntry](private val $ : Enum[A]) extends AnyVal {
    def withPrefixCaseInsensitive(s: String): Seq[A] =
      $.values.filter(_.entryName.startsWithCaseInsensitive(s))
    def ordinal(a: A): Int =
      $.values.findIndex(a.==).getOrThrow(s"Could not find <$a> in <${$.values}>")
    def ordering: Ordering[A] = {
      val ordinals = $.values.zipWithIndex.toMap
      Ordering by ordinals
    }
    def random(random: Random): A = random.select($.values)
    def gen: Gen[A] = MoreGen.oneOf($.values)
  }
}
