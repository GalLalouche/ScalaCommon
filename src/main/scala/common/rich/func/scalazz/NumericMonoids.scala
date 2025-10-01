package common.rich.func.scalazz

import scalaz.Monoid

object NumericMonoids {
  implicit def SumNumeric[N](implicit ev: Numeric[N]): Monoid[N] = new Monoid[N] {
    override def zero = ev.fromInt(0)
    override def append(f1: N, f2: => N) = ev.plus(f1, f2)
  }
  implicit def ProductNumeric[N](implicit ev: Numeric[N]): Monoid[N] = new Monoid[N] {
    override def zero = ev.fromInt(1)
    override def append(f1: N, f2: => N) = ev.times(f1, f2)
  }
}
