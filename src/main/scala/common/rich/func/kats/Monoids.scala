package common.rich.func.kats

import cats.{Applicative, Order}
import cats.kernel.{CommutativeMonoid, CommutativeSemigroup}

import common.rich.func.kats.ToMoreApplyOps.toMoreApplyOps

object Monoids {
  case class Min[A](getMin: A) extends AnyVal
  case class Max[A](getMax: A) extends AnyVal
  /** Technically not commutative since it's left biased, but meh. */
  implicit def MinCommutativeMonoid[A: Order]: CommutativeSemigroup[Min[A]] =
    CommutativeSemigroup.instance((x, y) => Min(Order.min(x.getMin, y.getMin)))
  implicit def MaxCommutativeMonoid[A: Order]: CommutativeSemigroup[Max[A]] =
    CommutativeSemigroup.instance((x, y) => Max(Order.max(x.getMax, y.getMax)))
  implicit def GUnitCommutativeMonoid[G[_]: Applicative]: CommutativeMonoid[G[Unit]] =
    CommutativeMonoid.instance[G[Unit]](Applicative[G].unit, _ *>> _)
}
