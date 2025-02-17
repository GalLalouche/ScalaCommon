package common.rich.func.scalazz

import scalaz.Monoid
import scalaz.syntax.ToMonoidOps

object ToMoreMonoidOps extends ToMonoidOps {
  implicit class monoidFilter[A](a: => A)(implicit ev: Monoid[A]) {
    def monoidFilter(b: Boolean): A = if (b) a else ev.zero
    def monoidFilter(p: A => Boolean): A = if (p(a)) a else ev.zero
  }
  implicit class toMoreMonoidOptionOps[A: Monoid](a: Option[A]) {
    def getOrZero: A = a.getOrElse(implicitly[Monoid[A]].zero)
  }
}
