package common.rich

import enumeratum.{Enum, EnumEntry}
import org.scalacheck.Gen

import scala.util.Random

import common.rich.RichRandom.richRandom
import common.rich.collections.RichSeq._
import common.rich.primitives.RichOption._

object RichEnumeratum {
  implicit class richEnumeratum[A <: EnumEntry](e: Enum[A]) {
    def withPrefixCaseInsensitive(s: String): Seq[A] = {
      val lowerCase = s.toLowerCase
      e.values.filter(_.entryName.toLowerCase.startsWith(lowerCase))
    }
    def ordinal(a: A): Int =
      e.values.findIndex(a.==).getOrThrow(s"Could not find <$a> in <${e.values}>")
    def ordering: Ordering[A] = Ordering by ordinal
    def random(random: Random): A = random.select(e.values)
    def gen: Gen[A] = Gen.choose(0, e.values.length - 1).map(e.values)
  }
}
