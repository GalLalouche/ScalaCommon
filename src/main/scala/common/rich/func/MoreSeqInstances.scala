package common.rich.func

import scala.language.higherKinds
import scalaz.{Foldable, MonadPlus, Monoid}

trait MoreSeqInstances {
  implicit object SeqMonadPlus extends MonadPlus[Seq] with Foldable[Seq] {
    override def bind[A, B](fa: Seq[A])(f: A => Seq[B]) = fa flatMap f
    override def point[A](a: => A) = Seq(a)
    override def empty[A] = Seq.empty
    override def plus[A](a: Seq[A], b: => Seq[A]) = a ++ b

    // Optimized implementation
    override def foldRight[A, B](fa: Seq[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f(_, _))
    override def foldMap[A, B: Monoid](fa: Seq[A])(f: A => B): B =
      fa.map(f).fold(Monoid[B].zero)(Monoid[B].append(_, _))
    override def foldLeft[A, B](fa: Seq[A], z: B)(f: (B, A) => B) = fa.foldLeft(z)(f)
  }
}
