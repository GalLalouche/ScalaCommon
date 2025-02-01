package common.test

import org.scalacheck.Gen

import scalaz.Monad

object GenInstances {
  implicit object MonadGen extends Monad[Gen] {
    override def point[A](a: => A): Gen[A] = Gen.const(a)
    override def map[A, B](fa: Gen[A])(f: A => B): Gen[B] = fa.map(f)
    override def bind[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)
  }
}
