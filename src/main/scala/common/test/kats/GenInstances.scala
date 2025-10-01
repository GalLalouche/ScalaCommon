package common.test.kats

import cats.Monad
import org.scalacheck.Gen

object GenInstances {
  implicit object MonadGen extends Monad[Gen] {
    override def pure[A](a: A): Gen[A] = Gen.const(a)
    override def map[A, B](fa: Gen[A])(f: A => B): Gen[B] = fa.map(f)
    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)
    override def tailRecM[A, B](a: A)(f: A => Gen[Either[A, B]]): Gen[B] = Gen.tailRecM(a)(f)
  }
}
